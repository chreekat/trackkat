/* Windows plumbing for trackkat.exe: publishing poses to games, the control
 * block shared with our NPClient DLL, global hotkeys and joystick buttons,
 * and the registry. */

#include <windows.h>
#include <hidsdi.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "trackkat_shared.h"

/* ---- FreeTrack shared memory ------------------------------------------- */

static HANDLE ft_mapping;
static volatile FTHeap *ft_heap;
static HANDLE control_mapping;
static volatile TrackKatControl *control;

/* Create (or open) the FreeTrack heap and our control block. Returns 0 on
 * success. */
int hts_shared_open(void)
{
    HANDLE mutex = CreateMutexA(NULL, FALSE, FREETRACK_MUTEX);
    if (mutex)
        CloseHandle(mutex);

    ft_mapping = CreateFileMappingA(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0,
                                    sizeof(FTHeap), FREETRACK_HEAP);
    if (!ft_mapping)
        return (int)GetLastError();
    ft_heap = MapViewOfFile(ft_mapping, FILE_MAP_WRITE, 0, 0, sizeof(FTHeap));
    if (!ft_heap)
        return (int)GetLastError();

    control_mapping = CreateFileMappingA(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0,
                                         sizeof(TrackKatControl), TRACKKAT_CONTROL);
    if (!control_mapping)
        return (int)GetLastError();
    control = MapViewOfFile(control_mapping, FILE_MAP_WRITE, 0, 0, sizeof(TrackKatControl));
    if (!control)
        return (int)GetLastError();
    return 0;
}

static void store_float(volatile float *place, float value)
{
    union { float f; LONG l; } u;
    u.f = value;
    InterlockedExchange((volatile LONG *)place, u.l);
}

/* Publish a pose: angles in radians, positions in mm, in FreeTrack's
 * conventions. */
void hts_publish(float yaw, float pitch, float roll, float x, float y, float z)
{
    volatile FTData *d = &ft_heap->data;
    store_float(&d->Yaw, yaw);
    store_float(&d->Pitch, pitch);
    store_float(&d->Roll, roll);
    store_float(&d->X, x);
    store_float(&d->Y, y);
    store_float(&d->Z, z);
    store_float(&d->RawYaw, yaw);
    store_float(&d->RawPitch, pitch);
    store_float(&d->RawRoll, roll);
    store_float(&d->RawX, x);
    store_float(&d->RawY, y);
    store_float(&d->RawZ, z);
    InterlockedIncrement((volatile LONG *)&d->DataID);
}

/* Recenter requests from games, counted since the block was created. */
long hts_recenter_count(void)
{
    return InterlockedCompareExchange(&control->recenter_requests, 0, 0);
}

/* How many times games have read a pose, counted since the block was
 * created. */
long hts_game_reads(void)
{
    return InterlockedCompareExchange(&control->game_reads, 0, 0);
}

/* How many games have started head tracking and are still running. Slots
 * of games that have exited, perhaps by crashing, are freed. */
int hts_games_tracking(void)
{
    int n = 0;
    for (int i = 0; i < TRACKKAT_MAX_GAMES; i++) {
        long pid = InterlockedCompareExchange(&control->games[i], 0, 0);
        if (pid == 0)
            continue;
        DWORD code = 0;
        HANDLE p = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, (DWORD)pid);
        int alive = p && GetExitCodeProcess(p, &code) && code == STILL_ACTIVE;
        if (p)
            CloseHandle(p);
        if (alive)
            n++;
        else
            InterlockedCompareExchange(&control->games[i], 0, pid);
    }
    return n;
}

/* ---- Single instance ---------------------------------------------------- */

/* Nonzero if this process now holds the trackkat instance mutex, which our
 * NPClient DLL checks to see whether trackkat is running. */
int hts_single_instance(void)
{
    HANDLE m = CreateMutexA(NULL, TRUE, TRACKKAT_RUNNING_MUTEX);
    return m != NULL && GetLastError() != ERROR_ALREADY_EXISTS;
}

/* ---- Registry ------------------------------------------------------------ */

/* Point games at the NPClient DLLs in the given directory, which must end in
 * a backslash. Returns 0 on success. */
int hts_set_npclient_path(const wchar_t *dir)
{
    HKEY key;
    LONG rc = RegCreateKeyExW(HKEY_CURRENT_USER,
                              L"Software\\NaturalPoint\\NATURALPOINT\\NPClient Location",
                              0, NULL, 0, KEY_SET_VALUE, NULL, &key, NULL);
    if (rc != ERROR_SUCCESS)
        return (int)rc;
    rc = RegSetValueExW(key, L"Path", 0, REG_SZ, (const BYTE *)dir,
                        (DWORD)((wcslen(dir) + 1) * sizeof(wchar_t)));
    RegCloseKey(key);
    return (int)rc;
}

/* ---- Hotkeys and joystick buttons ---------------------------------------- */

/* Keys are watched with a low-level keyboard hook rather than RegisterHotKey,
 * which refuses F12 (reserved for debuggers) and swallows the key, hiding it
 * from games bound to the same key.
 *
 * Joysticks are read through Raw Input, as HID reports. winmm sees only 32
 * buttons, fewer than many HOTAS throttles have; DirectInput must be polled,
 * and re-enumerated (slowly) to find controllers plugged in later. Raw Input
 * delivers each report as it arrives, from every controller however many
 * buttons it has, including those plugged in later or re-enumerated after
 * sleep, and leaves the buttons to games too. Keys and joysticks are both
 * handled on one thread, which owns the joystick table. */

#define HOTKEY_RECENTER 1
#define HOTKEY_PAUSE 2

/* Modifier flags, as RegisterHotKey numbers them. */
#define HK_ALT 0x1
#define HK_CTRL 0x2
#define HK_SHIFT 0x4
#define HK_WIN 0x8

#define MAX_BINDINGS 32
#define NAME_LEN 128 /* a joystick name and its terminator */
#define MAX_BUTTONS 256 /* as maxButton in src/TrackKat/Config.hs */
#define MAX_JOYSTICKS 16
#define MAX_EVENTS 64

/* A key or joystick button, and what pressing it does. */
typedef struct {
    LONG action; /* HOTKEY_RECENTER or HOTKEY_PAUSE */
    LONG key;    /* mods << 16 | vk, or 0 for a joystick button */
    wchar_t joystick[NAME_LEN];
    int index;   /* which of the joysticks with that name, from 1 */
    int button;
} Binding;

static SRWLOCK bindings_lock = SRWLOCK_INIT;
static Binding bindings[MAX_BINDINGS];
static int binding_count;
/* Bindings being gathered by hts_bind_*, until hts_bindings_apply. */
static Binding staged[MAX_BINDINGS];
static int staged_count;

static volatile LONG hotkey_presses;
static volatile LONG hotkey_status; /* 0 starting, 1 started */
static DWORD keyboard_error, joystick_error;
static DWORD held_vk; /* a bound key now held down, so repeats are ignored */

/* Start gathering a new set of bindings; the old ones stay in force until
 * hts_bindings_apply. Bindings past MAX_BINDINGS are ignored. */
void hts_bindings_clear(void)
{
    staged_count = 0;
}

void hts_bind_key(int action, unsigned mods, unsigned vk)
{
    if (staged_count == MAX_BINDINGS)
        return;
    Binding *b = &staged[staged_count++];
    b->action = action;
    b->key = (LONG)(mods << 16 | vk);
    b->joystick[0] = 0;
    b->index = 0;
    b->button = 0;
}

void hts_bind_button(int action, const wchar_t *joystick, int index, int button)
{
    if (staged_count == MAX_BINDINGS)
        return;
    Binding *b = &staged[staged_count++];
    b->action = action;
    b->key = 0;
    lstrcpynW(b->joystick, joystick, NAME_LEN);
    b->index = index;
    b->button = button;
}

/* Put the gathered bindings in force, whether or not keys and joysticks
 * are being watched yet. */
void hts_bindings_apply(void)
{
    AcquireSRWLockExclusive(&bindings_lock);
    memcpy(bindings, staged, sizeof staged);
    binding_count = staged_count;
    ReleaseSRWLockExclusive(&bindings_lock);
}

static UINT modifiers_held(void)
{
    UINT m = 0;
    if (GetAsyncKeyState(VK_MENU) & 0x8000)
        m |= HK_ALT;
    if (GetAsyncKeyState(VK_CONTROL) & 0x8000)
        m |= HK_CTRL;
    if (GetAsyncKeyState(VK_SHIFT) & 0x8000)
        m |= HK_SHIFT;
    if ((GetAsyncKeyState(VK_LWIN) | GetAsyncKeyState(VK_RWIN)) & 0x8000)
        m |= HK_WIN;
    return m;
}

static LRESULT CALLBACK keyboard_hook(int code, WPARAM wparam, LPARAM lparam)
{
    if (code == HC_ACTION) {
        const KBDLLHOOKSTRUCT *k = (const KBDLLHOOKSTRUCT *)lparam;
        if (wparam == WM_KEYDOWN || wparam == WM_SYSKEYDOWN) {
            if (k->vkCode != held_vk) {
                LONG pressed = (LONG)(modifiers_held() << 16 | k->vkCode);
                LONG hit = 0;
                AcquireSRWLockShared(&bindings_lock);
                for (int i = 0; i < binding_count; i++)
                    if (bindings[i].key == pressed)
                        hit |= bindings[i].action;
                ReleaseSRWLockShared(&bindings_lock);
                if (hit) {
                    held_vk = k->vkCode;
                    InterlockedOr(&hotkey_presses, hit);
                }
            }
        } else if ((wparam == WM_KEYUP || wparam == WM_SYSKEYUP) && k->vkCode == held_vk) {
            held_vk = 0;
        }
    }
    return CallNextHookEx(NULL, code, wparam, lparam);
}

#define JOY_CONNECTED 1
#define JOY_DISCONNECTED 2
#define JOY_PRESSED 3

typedef struct {
    HANDLE device; /* Raw Input's handle for it; NULL for a free slot */
    wchar_t path[512];
    wchar_t name[NAME_LEN];
    PHIDP_PREPARSED_DATA preparsed;
    PHIDP_BUTTON_CAPS ranges; /* the buttons each report carries */
    USHORT range_count;
    uint8_t held[MAX_BUTTONS / 8]; /* bit b - 1 set while button b is down */
} Joystick;

static Joystick joysticks[MAX_JOYSTICKS];

typedef struct {
    int kind; /* JOY_CONNECTED, JOY_DISCONNECTED or JOY_PRESSED */
    wchar_t name[NAME_LEN];
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
        lstrcpynW(e->name, j->name, NAME_LEN);
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

static void forget_joystick(Joystick *j)
{
    free(j->preparsed);
    free(j->ranges);
    memset(j, 0, sizeof *j);
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

    /* Opened for neither reading nor writing, it answers questions about
     * itself even while a game has it open. */
    HANDLE h = CreateFileW(j->path, 0, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING, 0, NULL);
    if (h != INVALID_HANDLE_VALUE) {
        if (!HidD_GetProductString(h, j->name, sizeof j->name - sizeof *j->name))
            j->name[0] = 0;
        CloseHandle(h);
    }
    tidy_name(j->name);
    if (!j->name[0]) {
        RID_DEVICE_INFO info;
        info.cbSize = sizeof info;
        size = sizeof info;
        if ((int)GetRawInputDeviceInfoW(device, RIDI_DEVICEINFO, &info, &size) > 0 && info.dwType == RIM_TYPEHID)
            wsprintfW(j->name, L"Joystick %04X:%04X", (unsigned)info.hid.dwVendorId, (unsigned)info.hid.dwProductId);
        else
            lstrcpyW(j->name, L"Joystick");
    }

    j->device = device;
    report_event(JOY_CONNECTED, j, 0);
    return j;
}

/* What pressing this joystick button does. */
static LONG button_actions(const Joystick *j, int button)
{
    int index = joystick_index(j);
    LONG hit = 0;
    AcquireSRWLockShared(&bindings_lock);
    for (int i = 0; i < binding_count; i++) {
        const Binding *b = &bindings[i];
        if (b->key == 0 && b->button == button && b->index == index && same_name(b->joystick, j->name))
            hit |= b->action;
    }
    ReleaseSRWLockShared(&bindings_lock);
    return hit;
}

/* Act on the buttons newly down in one input report. A report carries only
 * the buttons its report ID covers; the others keep their state. */
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

    LONG hit = 0;
    for (ULONG i = 0; i < n; i++) {
        unsigned b = down[i];
        if (b < 1 || b > MAX_BUTTONS)
            continue;
        uint8_t bit = (uint8_t)(1u << ((b - 1) % 8));
        held[(b - 1) / 8] |= bit;
        if (!(j->held[(b - 1) / 8] & bit)) {
            hit |= button_actions(j, (int)b);
            report_event(JOY_PRESSED, j, (int)b);
        }
    }
    memcpy(j->held, held, sizeof held);
    if (hit)
        InterlockedOr(&hotkey_presses, hit);
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
            report_event(JOY_DISCONNECTED, j, 0);
            forget_joystick(j);
        }
        return 0;
    }
    return DefWindowProcW(w, msg, wparam, lparam);
}

/* Ask for every joystick's input, and to hear of joysticks coming and
 * going, through this thread's messages. Returns 0 or a Windows error
 * code. */
static DWORD watch_joysticks(void)
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

static DWORD WINAPI hotkey_thread(LPVOID unused)
{
    (void)unused;
    HHOOK hook = SetWindowsHookExW(WH_KEYBOARD_LL, keyboard_hook, GetModuleHandleW(NULL), 0);
    keyboard_error = hook ? 0 : GetLastError();
    joystick_error = watch_joysticks();
    InterlockedExchange(&hotkey_status, 1);
    if (keyboard_error && joystick_error)
        return 0;

    /* The hook and the joystick window run only while this thread pumps
     * messages. */
    MSG msg;
    while (GetMessageW(&msg, NULL, 0, 0) > 0)
        DispatchMessageW(&msg);
    if (hook)
        UnhookWindowsHookEx(hook);
    return 0;
}

/* Start watching the keyboard and joysticks for bound keys and buttons on a
 * thread of their own. Returns 0, or a Windows error code if the keyboard
 * can't be watched; sets *joysticks_error likewise. */
int hts_hotkeys_start(int *joysticks_error)
{
    HANDLE t = CreateThread(NULL, 0, hotkey_thread, NULL, 0, NULL);
    if (!t) {
        *joysticks_error = (int)GetLastError();
        return *joysticks_error;
    }
    CloseHandle(t);
    while (InterlockedCompareExchange(&hotkey_status, 0, 0) == 0)
        Sleep(1);
    *joysticks_error = (int)joystick_error;
    return (int)keyboard_error;
}

/* Actions pressed for since the last call: bit 1 recenter, bit 2 pause. */
int hts_hotkeys_take(void)
{
    return (int)InterlockedExchange(&hotkey_presses, 0);
}
