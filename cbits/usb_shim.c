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
