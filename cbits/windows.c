/* Windows plumbing for trackkat.exe: publishing poses to games, the control
 * block shared with our NPClient DLL, global hotkeys and joystick buttons,
 * and the registry. */

#include <windows.h>
#include <stdint.h>
#include <string.h>

#include "joystick.h"
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
 * from games bound to the same key. Joysticks (joystick.c) are watched on
 * the same thread. */

/* Modifier flags, as RegisterHotKey numbers them. */
#define HK_ALT 0x1
#define HK_CTRL 0x2
#define HK_SHIFT 0x4
#define HK_WIN 0x8

#define MAX_BINDINGS 32

/* A key or joystick button, and what pressing it does. */
typedef struct {
    LONG action; /* HOTKEY_RECENTER or HOTKEY_PAUSE */
    LONG key;    /* mods << 16 | vk, or 0 for a joystick button */
    wchar_t joystick[JOYSTICK_NAME_LEN];
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
    lstrcpynW(b->joystick, joystick, JOYSTICK_NAME_LEN);
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

LONG hotkeys_for_button(const wchar_t *joystick, int index, int button)
{
    LONG hit = 0;
    AcquireSRWLockShared(&bindings_lock);
    for (int i = 0; i < binding_count; i++) {
        const Binding *b = &bindings[i];
        if (b->key == 0 && b->button == button && b->index == index
            && CompareStringOrdinal(b->joystick, -1, joystick, -1, TRUE) == CSTR_EQUAL)
            hit |= b->action;
    }
    ReleaseSRWLockShared(&bindings_lock);
    return hit;
}

void hotkeys_press(LONG actions)
{
    InterlockedOr(&hotkey_presses, actions);
}

static DWORD WINAPI hotkey_thread(LPVOID unused)
{
    (void)unused;
    HHOOK hook = SetWindowsHookExW(WH_KEYBOARD_LL, keyboard_hook, GetModuleHandleW(NULL), 0);
    keyboard_error = hook ? 0 : GetLastError();
    joystick_error = joysticks_watch();
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
