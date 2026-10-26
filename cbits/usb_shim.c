#include <libusb-1.0/libusb.h>
#include <stdint.h>

/* Open the first attached device with the given vendor id whose product id is
 * in pids[]. Earlier entries in pids[] take precedence.
 *
 * Returns 0 on success (storing the handle and matched product id),
 * LIBUSB_ERROR_NOT_FOUND if no such device is attached, or the libusb error
 * from opening it. On Windows, LIBUSB_ERROR_NOT_SUPPORTED from the open means
 * the device is bound to a driver libusb cannot use (i.e. not WinUSB). */
int hts_open_first(libusb_context *ctx, uint16_t vid,
                   const uint16_t *pids, int npids,
                   uint16_t *found_pid, libusb_device_handle **out)
{
  libusb_device **list;
  ssize_t n = libusb_get_device_list(ctx, &list);
  if (n < 0)
    return (int)n;

  int rc = LIBUSB_ERROR_NOT_FOUND;
  for (int p = 0; p < npids && rc == LIBUSB_ERROR_NOT_FOUND; ++p) {
    for (ssize_t i = 0; i < n; ++i) {
      struct libusb_device_descriptor d;
      if (libusb_get_device_descriptor(list[i], &d) != 0)
        continue;
      if (d.idVendor == vid && d.idProduct == pids[p]) {
        rc = libusb_open(list[i], out);
        if (rc == 0)
          *found_pid = pids[p];
        break;
      }
    }
  }
  libusb_free_device_list(list, 1);
  return rc;
}

/* For diagnostics: report up to max attached devices' ids, and the result of
 * trying to open each one (0 = opened fine). Returns the number reported or
 * a libusb error. */
int hts_list(libusb_context *ctx, uint16_t *vids, uint16_t *pids,
             int *open_rcs, int max)
{
  libusb_device **list;
  ssize_t n = libusb_get_device_list(ctx, &list);
  if (n < 0)
    return (int)n;

  int count = 0;
  for (ssize_t i = 0; i < n && count < max; ++i) {
    struct libusb_device_descriptor d;
    if (libusb_get_device_descriptor(list[i], &d) != 0)
      continue;
    libusb_device_handle *h;
    int rc = libusb_open(list[i], &h);
    if (rc == 0)
      libusb_close(h);
    vids[count] = d.idVendor;
    pids[count] = d.idProduct;
    open_rcs[count] = rc;
    ++count;
  }
  libusb_free_device_list(list, 1);
  return count;
}

/* ---- Streaming bulk reads ------------------------------------------------
 *
 * Several reads stay queued with the device, each resubmitted as soon as it
 * completes, so the device always has somewhere to put data. All of these
 * functions, and so the transfer callbacks, run on one thread. */

#include <stdlib.h>
#include <string.h>

typedef struct hts_stream {
    libusb_context *ctx;
    struct libusb_transfer **xfers;
    int n;
    int active;   /* transfers queued with the device */
    int stopping;
    int error;    /* libusb error that ended a transfer, or 0 */
    unsigned char *pending; /* bytes received but not yet taken */
    size_t len, cap;
} hts_stream;

void hts_stream_close(hts_stream *s);

static void LIBUSB_CALL stream_callback(struct libusb_transfer *t)
{
    hts_stream *s = t->user_data;
    if (t->status == LIBUSB_TRANSFER_COMPLETED || t->status == LIBUSB_TRANSFER_TIMED_OUT) {
        size_t n = (size_t)t->actual_length;
        if (s->len + n > s->cap) {
            size_t cap = s->cap ? s->cap : 65536;
            while (s->len + n > cap)
                cap *= 2;
            unsigned char *p = realloc(s->pending, cap);
            if (!p) {
                s->error = LIBUSB_ERROR_NO_MEM;
                s->active--;
                return;
            }
            s->pending = p;
            s->cap = cap;
        }
        memcpy(s->pending + s->len, t->buffer, n);
        s->len += n;
        if (!s->stopping) {
            int rc = libusb_submit_transfer(t);
            if (rc == 0)
                return;
            s->error = rc;
        }
    } else if (t->status == LIBUSB_TRANSFER_NO_DEVICE) {
        s->error = LIBUSB_ERROR_NO_DEVICE;
    } else if (t->status != LIBUSB_TRANSFER_CANCELLED) {
        s->error = LIBUSB_ERROR_IO;
    }
    s->active--;
}

/* Start n reads of size bytes each on endpoint ep. Returns NULL and sets
 * *rc on failure. */
hts_stream *hts_stream_open(libusb_context *ctx, libusb_device_handle *h, unsigned char ep,
                            int n, int size, int *rc)
{
    hts_stream *s = calloc(1, sizeof *s);
    if (!s || !(s->xfers = calloc((size_t)n, sizeof *s->xfers))) {
        free(s);
        *rc = LIBUSB_ERROR_NO_MEM;
        return NULL;
    }
    s->ctx = ctx;
    s->n = n;
    for (int i = 0; i < n; i++) {
        struct libusb_transfer *t = libusb_alloc_transfer(0);
        unsigned char *buf = malloc((size_t)size);
        if (!t || !buf) {
            libusb_free_transfer(t);
            free(buf);
            *rc = LIBUSB_ERROR_NO_MEM;
            break;
        }
        libusb_fill_bulk_transfer(t, h, ep, buf, size, stream_callback, s, 0);
        t->flags = LIBUSB_TRANSFER_FREE_BUFFER;
        s->xfers[i] = t;
        *rc = libusb_submit_transfer(t);
        if (*rc != 0)
            break;
        s->active++;
    }
    if (s->active < n) {
        hts_stream_close(s);
        return NULL;
    }
    *rc = 0;
    return s;
}

/* Wait up to timeout_ms for data, and move up to cap received bytes into
 * out. Returns the number of bytes, or a libusb error once all data before
 * the error has been taken. */
int hts_stream_pump(hts_stream *s, int timeout_ms, unsigned char *out, int cap)
{
    if (s->len == 0 && s->active > 0) {
        struct timeval tv = {timeout_ms / 1000, (timeout_ms % 1000) * 1000};
        int rc = libusb_handle_events_timeout_completed(s->ctx, &tv, NULL);
        if (rc < 0 && rc != LIBUSB_ERROR_INTERRUPTED)
            return rc;
    }
    if (s->len == 0)
        return s->error ? s->error : (s->active > 0 ? 0 : LIBUSB_ERROR_IO);
    size_t n = s->len < (size_t)cap ? s->len : (size_t)cap;
    memcpy(out, s->pending, n);
    memmove(s->pending, s->pending + n, s->len - n);
    s->len -= n;
    return (int)n;
}

/* Cancel the reads, wait for them to finish, and free everything. */
void hts_stream_close(hts_stream *s)
{
    s->stopping = 1;
    for (int i = 0; i < s->n; i++)
        if (s->xfers[i])
            libusb_cancel_transfer(s->xfers[i]);
    for (int tries = 0; s->active > 0 && tries < 50; tries++) {
        struct timeval tv = {0, 100000};
        libusb_handle_events_timeout_completed(s->ctx, &tv, NULL);
    }
    /* Transfers still queued after this long belong to a dead device; leak
     * them rather than free memory libusb may still write to. */
    if (s->active == 0) {
        for (int i = 0; i < s->n; i++)
            libusb_free_transfer(s->xfers[i]);
        free(s->xfers);
        free(s->pending);
        free(s);
    }
}
