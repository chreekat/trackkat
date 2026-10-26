/* Windows plumbing for trackkat.exe: publishing poses to games, the control
 * block shared with our NPClient DLL, global hotkeys, and the registry. */

#include <windows.h>
#include <stdint.h>

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

/* ---- Global hotkeys ------------------------------------------------------ */

/* Watched with a low-level keyboard hook rather than RegisterHotKey, which
 * refuses F12 (reserved for debuggers) and swallows the key, hiding it from
 * games bound to the same key. */

#define HOTKEY_RECENTER 1
#define HOTKEY_PAUSE 2

/* Modifier flags, as RegisterHotKey numbers them. */
#define HK_ALT 0x1
#define HK_CTRL 0x2
#define HK_SHIFT 0x4
#define HK_WIN 0x8

static volatile LONG hotkey_presses;
/* Each binding packed as mods << 16 | vk, so it changes in one atomic step. */
static volatile LONG recenter_binding, pause_binding;
static volatile LONG hotkey_status; /* 0 starting, 1 running, 2 failed */
static volatile LONG hotkey_error;
static DWORD held_vk; /* a bound key now held down, so repeats are ignored */

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
                if (pressed == InterlockedCompareExchange(&recenter_binding, 0, 0))
                    hit |= HOTKEY_RECENTER;
                if (pressed == InterlockedCompareExchange(&pause_binding, 0, 0))
                    hit |= HOTKEY_PAUSE;
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

static DWORD WINAPI hotkey_thread(LPVOID unused)
{
    (void)unused;
    HHOOK hook = SetWindowsHookExW(WH_KEYBOARD_LL, keyboard_hook, GetModuleHandleW(NULL), 0);
    if (!hook) {
        InterlockedExchange(&hotkey_error, (LONG)GetLastError());
        InterlockedExchange(&hotkey_status, 2);
        return 0;
    }
    InterlockedExchange(&hotkey_status, 1);

    /* The hook runs only while this thread pumps messages. */
    MSG msg;
    while (GetMessageW(&msg, NULL, 0, 0) > 0) {
    }
    UnhookWindowsHookEx(hook);
    return 0;
}

/* Change the two hotkeys, whether or not they are being watched yet. */
void hts_hotkeys_rebind(unsigned rmods, unsigned rvk, unsigned pmods, unsigned pvk)
{
    InterlockedExchange(&recenter_binding, (LONG)(rmods << 16 | rvk));
    InterlockedExchange(&pause_binding, (LONG)(pmods << 16 | pvk));
}

/* Start watching for the two hotkeys on a thread of their own. Returns 0, or
 * a Windows error code if the keyboard can't be watched. */
int hts_hotkeys_start(unsigned rmods, unsigned rvk, unsigned pmods, unsigned pvk)
{
    hts_hotkeys_rebind(rmods, rvk, pmods, pvk);
    HANDLE t = CreateThread(NULL, 0, hotkey_thread, NULL, 0, NULL);
    if (!t)
        return (int)GetLastError();
    CloseHandle(t);
    while (InterlockedCompareExchange(&hotkey_status, 0, 0) == 0)
        Sleep(1);
    return hotkey_status == 1 ? 0 : (int)hotkey_error;
}

/* Hotkeys pressed since the last call: bit 1 recenter, bit 2 pause. */
int hts_hotkeys_take(void)
{
    return (int)InterlockedExchange(&hotkey_presses, 0);
}
