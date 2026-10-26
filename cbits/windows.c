/* Windows plumbing for trackkat.exe: publishing poses to games, the control
 * block shared with our NPClient DLL, global hotkeys, and the registry. */

#include <windows.h>
#include <stdint.h>

#include "trackkat_shared.h"

/* Windows 7's no-repeat flag, for toolchains whose headers target older
 * Windows. */
#ifndef MOD_NOREPEAT
#define MOD_NOREPEAT 0x4000
#endif

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

#define HOTKEY_RECENTER 1
#define HOTKEY_PAUSE 2

static volatile LONG hotkey_presses;
static UINT recenter_mods, recenter_vk, pause_mods, pause_vk;
static volatile LONG hotkey_status;

static DWORD WINAPI hotkey_thread(LPVOID unused)
{
    (void)unused;
    LONG status = 0;
    if (!RegisterHotKey(NULL, HOTKEY_RECENTER, recenter_mods | MOD_NOREPEAT, recenter_vk))
        status |= 1;
    if (!RegisterHotKey(NULL, HOTKEY_PAUSE, pause_mods | MOD_NOREPEAT, pause_vk))
        status |= 2;
    InterlockedExchange(&hotkey_status, status | 4);

    MSG msg;
    while (GetMessageW(&msg, NULL, 0, 0) > 0) {
        if (msg.message == WM_HOTKEY && (msg.wParam == HOTKEY_RECENTER || msg.wParam == HOTKEY_PAUSE))
            InterlockedOr(&hotkey_presses, (LONG)msg.wParam);
    }
    return 0;
}

/* Start listening for the two hotkeys on a thread of their own. Returns a
 * bitmask of hotkeys that could not be registered (1 recenter, 2 pause),
 * typically because another program already has them. */
int hts_hotkeys_start(unsigned rmods, unsigned rvk, unsigned pmods, unsigned pvk)
{
    recenter_mods = rmods;
    recenter_vk = rvk;
    pause_mods = pmods;
    pause_vk = pvk;
    HANDLE t = CreateThread(NULL, 0, hotkey_thread, NULL, 0, NULL);
    if (!t)
        return 3;
    CloseHandle(t);
    while (!(InterlockedCompareExchange(&hotkey_status, 0, 0) & 4))
        Sleep(1);
    return (int)(hotkey_status & 3);
}

/* Hotkeys pressed since the last call: bit 1 recenter, bit 2 pause. */
int hts_hotkeys_take(void)
{
    return (int)InterlockedExchange(&hotkey_presses, 0);
}
