/* Joystick buttons going down and coming up, for trackkat.exe's hotkeys, and
 * the joysticks coming and going, for trackkat to report.
 *
 * Joysticks are read through Raw Input, as HID reports. winmm sees only 32
 * buttons, fewer than many HOTAS throttles have; DirectInput must be polled,
 * and re-enumerated (slowly) to find controllers plugged in later. Raw Input
 * delivers each report as it arrives, from every controller however many
 * buttons it has, including those plugged in later or re-enumerated after
 * sleep, and leaves the buttons to games too. Everything here but
 * hts_joystick_event and query_name runs on the hotkey thread, which owns
 * the joystick table. */

#include <windows.h>
#include <hidsdi.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "joystick.h"

#define MAX_BUTTONS 256 /* as maxButton in src/TrackKat/Config.hs */
#define MAX_JOYSTICKS 16
#define MAX_EVENTS 64

#define JOY_CONNECTED 1
#define JOY_DISCONNECTED 2
#define JOY_PRESSED 3

/* Posted to the joystick window by query_name, with a NameQuery. */
#define WM_JOYSTICK_NAMED (WM_APP + 1)

/* Whether a joystick's name is known yet. */
enum naming { NAME_ASKED, NAME_KNOWN };

typedef struct {
    HANDLE device; /* Raw Input's handle for it; NULL for a free slot */
    unsigned ticket; /* tells its NameQuery from one for an earlier joystick in the slot */
    enum naming naming;
    wchar_t path[512];
    wchar_t name[JOYSTICK_NAME_LEN]; /* empty while NAME_ASKED */
    PHIDP_PREPARSED_DATA preparsed;
    PHIDP_BUTTON_CAPS ranges; /* the buttons each report carries */
    USHORT range_count;
    uint8_t held[MAX_BUTTONS / 8]; /* bit b - 1 set while button b is down */
} Joystick;

static Joystick joysticks[MAX_JOYSTICKS];
static unsigned tickets;
static HWND joysticks_hwnd;

/* A joystick's product name, asked of the device by query_name. */
typedef struct {
    HWND window;
    unsigned ticket;
    wchar_t path[512];
    wchar_t name[JOYSTICK_NAME_LEN]; /* empty if the device won't say */
} NameQuery;

typedef struct {
    int kind; /* JOY_CONNECTED, JOY_DISCONNECTED or JOY_PRESSED */
    wchar_t name[JOYSTICK_NAME_LEN];
    int index;
    int button;
} JoystickEvent;

/* Events for trackkat to report, oldest first; new ones are dropped while
 * it is full. */
static SRWLOCK events_lock = SRWLOCK_INIT;
static JoystickEvent events[MAX_EVENTS];
static int events_first, events_count;

static int same_name(const wchar_t *a, const wchar_t *b)
{
    return CompareStringOrdinal(a, -1, b, -1, TRUE) == CSTR_EQUAL;
}

/* Which of the attached joysticks with this one's name it is, from 1, in
 * the order of their device paths, which stay the same from one boot to
 * the next. */
static int joystick_index(const Joystick *j)
{
    int index = 1;
    for (int i = 0; i < MAX_JOYSTICKS; i++) {
        const Joystick *o = &joysticks[i];
        if (o != j && o->device && same_name(o->name, j->name) && wcscmp(o->path, j->path) < 0)
            index++;
    }
    return index;
}

static void report_event(int kind, const Joystick *j, int button)
{
    int index = joystick_index(j);
    AcquireSRWLockExclusive(&events_lock);
    if (events_count < MAX_EVENTS) {
        JoystickEvent *e = &events[(events_first + events_count++) % MAX_EVENTS];
        e->kind = kind;
        lstrcpynW(e->name, j->name, JOYSTICK_NAME_LEN);
        e->index = index;
        e->button = button;
    }
    ReleaseSRWLockExclusive(&events_lock);
}

/* The oldest joystick event not yet taken, as its kind (JOY_CONNECTED,
 * JOY_DISCONNECTED or JOY_PRESSED), with the joystick's name and index and
 * the button pressed; 0 if there is none. */
int hts_joystick_event(wchar_t *name, int name_len, int *index, int *button)
{
    int kind = 0;
    AcquireSRWLockExclusive(&events_lock);
    if (events_count > 0) {
        const JoystickEvent *e = &events[events_first];
        kind = e->kind;
        lstrcpynW(name, e->name, name_len);
        *index = e->index;
        *button = e->button;
        events_first = (events_first + 1) % MAX_EVENTS;
        events_count--;
    }
    ReleaseSRWLockExclusive(&events_lock);
    return kind;
}

/* Make a product name fit to write in trackkat.conf, where commas separate
 * bindings and # starts a comment, and trim its spaces. */
static void tidy_name(wchar_t *name)
{
    for (wchar_t *c = name; *c; c++)
        if (*c == L',' || *c == L'#' || *c < L' ')
            *c = L' ';
    wchar_t *start = name;
    while (*start == L' ')
        start++;
    size_t n = wcslen(start);
    while (n > 0 && start[n - 1] == L' ')
        n--;
    memmove(name, start, n * sizeof *name);
    name[n] = 0;
}

static Joystick *find_joystick(HANDLE device)
{
    for (int i = 0; i < MAX_JOYSTICKS; i++)
        if (joysticks[i].device == device)
            return &joysticks[i];
    return NULL;
}

static int is_held(const uint8_t *held, unsigned b)
{
    return held[(b - 1) / 8] >> ((b - 1) % 8) & 1;
}

/* Tell of the buttons that differ between what was held and what is: those
 * newly down, to report and to the hotkeys, and those let go, to the
 * hotkeys. */
static void tell_buttons(const Joystick *j, const uint8_t *was, const uint8_t *is)
{
    int index = joystick_index(j);
    for (unsigned b = 1; b <= MAX_BUTTONS; b++) {
        int down = is_held(is, b);
        if (down == is_held(was, b))
            continue;
        watched_button(j->name, index, (int)b, down);
        if (down)
            report_event(JOY_PRESSED, j, (int)b);
    }
}

/* Stop following a joystick; the buttons held on it come up. */
static void forget_joystick(Joystick *j)
{
    static const uint8_t none[MAX_BUTTONS / 8];
    if (j->device && j->naming == NAME_KNOWN)
        tell_buttons(j, j->held, none);
    free(j->preparsed);
    free(j->ranges);
    memset(j, 0, sizeof *j);
}

/* Give a joystick the product name its device told, or, if it told none, a
 * name made from its vendor and product IDs; it is then connected. */
static void name_joystick(Joystick *j, const wchar_t *product)
{
    lstrcpynW(j->name, product, JOYSTICK_NAME_LEN);
    tidy_name(j->name);
    if (!j->name[0]) {
        RID_DEVICE_INFO info;
        info.cbSize = sizeof info;
        UINT size = sizeof info;
        if ((int)GetRawInputDeviceInfoW(j->device, RIDI_DEVICEINFO, &info, &size) > 0 && info.dwType == RIM_TYPEHID)
            wsprintfW(j->name, L"Joystick %04X:%04X", (unsigned)info.hid.dwVendorId, (unsigned)info.hid.dwProductId);
        else
            lstrcpyW(j->name, L"Joystick");
    }
    j->naming = NAME_KNOWN;
    report_event(JOY_CONNECTED, j, 0);
}

/* On a pool thread: ask a device its product name, and post the answer to
 * the joystick window. */
static DWORD WINAPI query_name(LPVOID context)
{
    NameQuery *q = context;
    /* Opened for neither reading nor writing, it answers questions about
     * itself even while a game has it open. */
    HANDLE h = CreateFileW(q->path, 0, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING, 0, NULL);
    if (h != INVALID_HANDLE_VALUE) {
        if (!HidD_GetProductString(h, q->name, sizeof q->name - sizeof *q->name))
            q->name[0] = 0;
        CloseHandle(h);
    }
    if (!PostMessageW(q->window, WM_JOYSTICK_NAMED, 0, (LPARAM)q))
        free(q);
    return 0;
}

/* Find out a joystick's name off this thread, which runs the low-level
 * hooks, as a device can be slow to answer; or, if that can't be, name it
 * without asking the device. */
static void ask_name(Joystick *j)
{
    NameQuery *q = calloc(1, sizeof *q);
    if (q) {
        q->window = joysticks_hwnd;
        q->ticket = j->ticket;
        lstrcpynW(q->path, j->path, ARRAYSIZE(q->path));
        if (QueueUserWorkItem(query_name, q, WT_EXECUTELONGFUNCTION))
            return;
        free(q);
    }
    name_joystick(j, L"");
}

/* A joystick's name, as query_name found it, unless the joystick it was
 * asked for has gone. */
static void named(NameQuery *q)
{
    for (int i = 0; i < MAX_JOYSTICKS; i++) {
        Joystick *j = &joysticks[i];
        if (j->device && j->ticket == q->ticket && j->naming == NAME_ASKED)
            name_joystick(j, q->name);
    }
    free(q);
}

/* Start following a joystick Raw Input has just told of. NULL if there is
 * no room for it, or it can't be read. */
static Joystick *add_joystick(HANDLE device)
{
    Joystick *j = find_joystick(NULL);
    if (!j)
        return NULL;
    UINT size = ARRAYSIZE(j->path);
    if ((int)GetRawInputDeviceInfoW(device, RIDI_DEVICENAME, j->path, &size) <= 0)
        return NULL;

    size = 0;
    if (GetRawInputDeviceInfoW(device, RIDI_PREPARSEDDATA, NULL, &size) != 0 || size == 0)
        return NULL;
    j->preparsed = malloc(size);
    HIDP_CAPS caps;
    if (!j->preparsed
        || (int)GetRawInputDeviceInfoW(device, RIDI_PREPARSEDDATA, j->preparsed, &size) <= 0
        || HidP_GetCaps(j->preparsed, &caps) != HIDP_STATUS_SUCCESS) {
        forget_joystick(j);
        return NULL;
    }
    j->range_count = caps.NumberInputButtonCaps;
    if (j->range_count > 0) {
        j->ranges = calloc(j->range_count, sizeof *j->ranges);
        if (!j->ranges
            || HidP_GetButtonCaps(HidP_Input, j->ranges, &j->range_count, j->preparsed) != HIDP_STATUS_SUCCESS) {
            forget_joystick(j);
            return NULL;
        }
    }

    j->device = device;
    j->ticket = ++tickets;
    j->naming = NAME_ASKED;
    ask_name(j);
    return j;
}

/* Tell of the buttons that go down or come up in one input report. A report
 * carries only the buttons its report ID covers; the others keep their
 * state. */
static void read_report(Joystick *j, BYTE *report, ULONG length)
{
    USAGE down[MAX_BUTTONS];
    ULONG n = MAX_BUTTONS;
    if (HidP_GetUsages(HidP_Input, HID_USAGE_PAGE_BUTTON, 0, down, &n, j->preparsed, (PCHAR)report, length)
        != HIDP_STATUS_SUCCESS)
        return;

    uint8_t held[MAX_BUTTONS / 8];
    memcpy(held, j->held, sizeof held);
    for (USHORT r = 0; r < j->range_count; r++) {
        const HIDP_BUTTON_CAPS *c = &j->ranges[r];
        if (c->UsagePage != HID_USAGE_PAGE_BUTTON || c->ReportID != report[0])
            continue;
        unsigned first = c->IsRange ? c->Range.UsageMin : c->NotRange.Usage;
        unsigned last = c->IsRange ? c->Range.UsageMax : c->NotRange.Usage;
        for (unsigned b = first < 1 ? 1 : first; b <= last && b <= MAX_BUTTONS; b++)
            held[(b - 1) / 8] &= (uint8_t)~(1u << ((b - 1) % 8));
    }

    for (ULONG i = 0; i < n; i++) {
        unsigned b = down[i];
        if (b >= 1 && b <= MAX_BUTTONS)
            held[(b - 1) / 8] |= (uint8_t)(1u << ((b - 1) % 8));
    }
    if (j->naming == NAME_KNOWN)
        tell_buttons(j, j->held, held);
    memcpy(j->held, held, sizeof held);
}

static void read_input(HRAWINPUT input)
{
    static union {
        RAWINPUT input;
        BYTE bytes[16384];
    } buffer;
    UINT size = sizeof buffer;
    if ((int)GetRawInputData(input, RID_INPUT, &buffer, &size, sizeof(RAWINPUTHEADER)) <= 0)
        return;
    const RAWINPUT *in = &buffer.input;
    if (in->header.dwType != RIM_TYPEHID || !in->header.hDevice)
        return;
    Joystick *j = find_joystick(in->header.hDevice);
    if (!j)
        j = add_joystick(in->header.hDevice);
    if (!j)
        return;
    BYTE *reports = (BYTE *)in->data.hid.bRawData;
    for (DWORD i = 0; i < in->data.hid.dwCount; i++)
        read_report(j, reports + i * in->data.hid.dwSizeHid, in->data.hid.dwSizeHid);
}

static LRESULT CALLBACK joystick_window(HWND w, UINT msg, WPARAM wparam, LPARAM lparam)
{
    if (msg == WM_INPUT) {
        read_input((HRAWINPUT)lparam);
    } else if (msg == WM_INPUT_DEVICE_CHANGE) {
        HANDLE device = (HANDLE)lparam;
        Joystick *j = find_joystick(device);
        if (wparam == GIDC_ARRIVAL && !j) {
            add_joystick(device);
        } else if (wparam == GIDC_REMOVAL && j) {
            if (j->naming == NAME_KNOWN)
                report_event(JOY_DISCONNECTED, j, 0);
            forget_joystick(j);
        }
        return 0;
    } else if (msg == WM_JOYSTICK_NAMED) {
        named((NameQuery *)lparam);
        return 0;
    }
    return DefWindowProcW(w, msg, wparam, lparam);
}

DWORD joysticks_watch(void)
{
    WNDCLASSW wc = {0};
    wc.lpfnWndProc = joystick_window;
    wc.hInstance = GetModuleHandleW(NULL);
    wc.lpszClassName = L"TrackKatJoysticks";
    if (!RegisterClassW(&wc))
        return GetLastError();
    HWND w = CreateWindowExW(0, wc.lpszClassName, L"", 0, 0, 0, 0, 0, HWND_MESSAGE, NULL, wc.hInstance, NULL);
    if (!w)
        return GetLastError();
    joysticks_hwnd = w;

    /* Joysticks, gamepads and multi-axis controllers. INPUTSINK delivers
     * their input while other programs are in front; DEVNOTIFY tells of
     * each arrival and removal. */
    const USHORT usages[] = {HID_USAGE_GENERIC_JOYSTICK, HID_USAGE_GENERIC_GAMEPAD, 0x08};
    RAWINPUTDEVICE devices[ARRAYSIZE(usages)];
    for (size_t i = 0; i < ARRAYSIZE(usages); i++) {
        devices[i].usUsagePage = HID_USAGE_PAGE_GENERIC;
        devices[i].usUsage = usages[i];
        devices[i].dwFlags = RIDEV_INPUTSINK | RIDEV_DEVNOTIFY;
        devices[i].hwndTarget = w;
    }
    if (!RegisterRawInputDevices(devices, ARRAYSIZE(devices), sizeof devices[0]))
        return GetLastError();

    /* Those already attached. */
    UINT count = 0;
    if (GetRawInputDeviceList(NULL, &count, sizeof(RAWINPUTDEVICELIST)) != 0 || count == 0)
        return 0;
    RAWINPUTDEVICELIST *list = calloc(count, sizeof *list);
    int found = list ? (int)GetRawInputDeviceList(list, &count, sizeof *list) : 0;
    for (int i = 0; i < found; i++) {
        RID_DEVICE_INFO info;
        UINT size = info.cbSize = sizeof info;
        if (list[i].dwType != RIM_TYPEHID
            || (int)GetRawInputDeviceInfoW(list[i].hDevice, RIDI_DEVICEINFO, &info, &size) <= 0
            || info.hid.usUsagePage != HID_USAGE_PAGE_GENERIC)
            continue;
        for (size_t u = 0; u < ARRAYSIZE(usages); u++)
            if (info.hid.usUsage == usages[u] && !find_joystick(list[i].hDevice))
                add_joystick(list[i].hDevice);
    }
    free(list);
    return 0;
}
