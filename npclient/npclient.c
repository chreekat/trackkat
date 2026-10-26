/* NPClient.dll / NPClient64.dll: what games load to get TrackIR data.
 *
 * It reads the pose trackkat.exe publishes in FreeTrack shared memory, and
 * starts trackkat.exe (from the DLL's own folder) if it isn't running.
 *
 * Derived from OpenTrack's contrib/npclient/npclient.c (ISC licence), by
 * uglyDwarf of linuxtrack (MIT licence). The checksum, the data scrambling
 * and the signature are what NaturalPoint-aware games check for. */

#include <windows.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "../cbits/trackkat_shared.h"

#define NP_EXPORT(t) __declspec(dllexport) t __stdcall
#define NP_AXIS_MAX 16383
#define PI 3.14159265358979323846

enum { NPCLIENT_STATUS_OK = 0, NPCLIENT_STATUS_DISABLED = 1 };

/* A pose older than this means trackkat has stopped publishing. */
#define STALE_MS 1000

typedef struct tir_data {
    short status;
    short frame;
    unsigned cksum;
    float roll, pitch, yaw;
    float tx, ty, tz;
    float padding[9];
} tir_data_t;

typedef struct tir_signature {
    char DllSignature[200];
    char AppSignature[200];
} tir_signature_t;

#include "game_keys.h"

static HMODULE self;
static HANDLE log_file = INVALID_HANDLE_VALUE;
static HANDLE heap_mapping, control_mapping;
static volatile FTHeap *heap;
static volatile TrackKatControl *control;
static const unsigned char *key;
static bool launch_attempted;
static unsigned last_data_id;
static ULONGLONG last_data_change;

/* Append a line to npclient.log beside the DLL, so we can see what a game
 * asks for. */
static void np_log(const char *fmt, ...)
{
    if (log_file == INVALID_HANDLE_VALUE)
        return;
    char line[512];
    SYSTEMTIME t;
    GetLocalTime(&t);
    int n = snprintf(line, sizeof line, "%02d:%02d:%02d.%03d ", t.wHour, t.wMinute, t.wSecond, t.wMilliseconds);
    va_list ap;
    va_start(ap, fmt);
    n += vsnprintf(line + n, sizeof line - (size_t)n - 2, fmt, ap);
    va_end(ap);
    if (n > (int)sizeof line - 2)
        n = (int)sizeof line - 2;
    line[n++] = '\r';
    line[n++] = '\n';
    DWORD written;
    WriteFile(log_file, line, (DWORD)n, &written, NULL);
}

/* Log only the first few calls of a kind. */
#define LOG_FIRST(limit, ...)                                                  \
    do {                                                                       \
        static int logged_;                                                    \
        if (logged_ < (limit)) {                                               \
            logged_++;                                                         \
            np_log(__VA_ARGS__);                                                 \
        }                                                                      \
    } while (0)

static void open_log(void)
{
    wchar_t path[MAX_PATH];
    DWORD n = GetModuleFileNameW(self, path, MAX_PATH);
    wchar_t *slash = n && n < MAX_PATH ? wcsrchr(path, L'\\') : NULL;
    if (!slash || (size_t)(slash + 1 - path) + wcslen(L"npclient.log") >= MAX_PATH)
        return;
    wcscpy(slash + 1, L"npclient.log");
    log_file = CreateFileW(path, FILE_APPEND_DATA, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL,
                           OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
    char exe[MAX_PATH];
    GetModuleFileNameA(NULL, exe, MAX_PATH);
    np_log("loaded (%d-bit) into %s", (int)sizeof(void *) * 8, exe);
}

static void unregister_game(void);

BOOL WINAPI DllMain(HINSTANCE instance, DWORD reason, LPVOID reserved)
{
    (void)reserved;
    if (reason == DLL_PROCESS_ATTACH) {
        self = instance;
        DisableThreadLibraryCalls(instance);
        open_log();
    } else if (reason == DLL_PROCESS_DETACH) {
        np_log("unloaded");
        unregister_game();
        if (log_file != INVALID_HANDLE_VALUE)
            CloseHandle(log_file);
        if (heap)
            UnmapViewOfFile((void *)heap);
        if (control)
            UnmapViewOfFile((void *)control);
        if (heap_mapping)
            CloseHandle(heap_mapping);
        if (control_mapping)
            CloseHandle(control_mapping);
    }
    return TRUE;
}

/* Start trackkat.exe from this DLL's folder, unless it is already running,
 * telling it a game started it: it then shows only in the system tray, with
 * no console window to take focus from the game, and quits once games are
 * done with it. Tried once per process. */
static void ensure_trackkat(void)
{
    if (launch_attempted)
        return;
    launch_attempted = true;

    HANDLE running = OpenMutexA(SYNCHRONIZE, FALSE, TRACKKAT_RUNNING_MUTEX);
    if (running) {
        CloseHandle(running);
        np_log("trackkat is running");
        return;
    }

    wchar_t path[MAX_PATH];
    DWORD n = GetModuleFileNameW(self, path, MAX_PATH);
    if (n == 0 || n >= MAX_PATH)
        return;
    wchar_t *slash = wcsrchr(path, L'\\');
    if (!slash || (size_t)(slash + 1 - path) + wcslen(L"trackkat.exe") >= MAX_PATH)
        return;
    wcscpy(slash + 1, L"trackkat.exe");
    *slash = 0;
    wchar_t dir[MAX_PATH];
    wcscpy(dir, path);
    *slash = L'\\';

    wchar_t command[MAX_PATH + 16];
    _snwprintf(command, MAX_PATH + 16, L"\"%ls\" --by-game", path);
    command[MAX_PATH + 15] = 0;

    STARTUPINFOW si;
    PROCESS_INFORMATION pi;
    memset(&si, 0, sizeof si);
    si.cb = sizeof si;
    if (CreateProcessW(path, command, NULL, NULL, FALSE, CREATE_NO_WINDOW, NULL, dir, &si, &pi)) {
        np_log("started trackkat.exe");
        CloseHandle(pi.hThread);
        CloseHandle(pi.hProcess);
    } else {
        np_log("could not start trackkat.exe (error %lu)", GetLastError());
    }
}

static bool registered;

/* Record this process among the games tracking, for trackkat to watch. */
static void register_game(void)
{
    if (registered || !control)
        return;
    long pid = (long)GetCurrentProcessId();
    for (int i = 0; i < TRACKKAT_MAX_GAMES; i++)
        if (InterlockedCompareExchange(&control->games[i], pid, 0) == 0) {
            registered = true;
            return;
        }
    LOG_FIRST(1, "no free slot to register this game in");
}

static void unregister_game(void)
{
    if (!registered || !control)
        return;
    long pid = (long)GetCurrentProcessId();
    for (int i = 0; i < TRACKKAT_MAX_GAMES; i++)
        InterlockedCompareExchange(&control->games[i], 0, pid);
    registered = false;
}

static bool map_shared(void)
{
    if (heap && control)
        return true;
    if (!heap) {
        heap_mapping = CreateFileMappingA(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0,
                                          sizeof(FTHeap), FREETRACK_HEAP);
        if (heap_mapping)
            heap = MapViewOfFile(heap_mapping, FILE_MAP_WRITE, 0, 0, sizeof(FTHeap));
    }
    if (!control) {
        control_mapping = CreateFileMappingA(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0,
                                             sizeof(TrackKatControl), TRACKKAT_CONTROL);
        if (control_mapping)
            control = MapViewOfFile(control_mapping, FILE_MAP_WRITE, 0, 0, sizeof(TrackKatControl));
    }
    if (!(heap && control))
        LOG_FIRST(3, "could not map shared memory (error %lu)", GetLastError());
    return heap && control;
}

/* TrackIR's checksum over the pose record. */
static unsigned cksum(unsigned char buf[], unsigned size)
{
    int rounds = size >> 2;
    int rem = size % 4;
    int c = size;
    int a0, a2;

    if (size == 0 || buf == NULL)
        return 0;

    while (rounds != 0) {
        short lo, hi; /* memcpy: reading the record through a short pointer breaks strict aliasing */
        memcpy(&lo, buf, 2);
        memcpy(&hi, buf + 2, 2);
        a0 = lo;
        a2 = hi;
        buf += 4;
        c += a0;
        a2 ^= (c << 5);
        a2 <<= 11;
        c ^= a2;
        c += (c >> 11);
        --rounds;
    }
    switch (rem) {
    case 3:
        a0 = *(short int *)buf;
        a2 = *(signed char *)(buf + 2);
        c += a0;
        a2 = (a2 << 2) ^ c;
        c ^= (a2 << 16);
        a2 = (c >> 11);
        break;
    case 2:
        a2 = *(short int *)buf;
        c += a2;
        c ^= (c << 11);
        a2 = (c >> 17);
        break;
    case 1:
        a2 = *(signed char *)(buf);
        c += a2;
        c ^= (c << 10);
        a2 = (c >> 1);
        break;
    default:
        break;
    }
    if (rem != 0)
        c += a2;

    c ^= (c << 3);
    c += (c >> 5);
    c ^= (c << 4);
    c += (c >> 17);
    c ^= (c << 25);
    c += (c >> 6);

    return (unsigned)c;
}

/* Scramble the pose record with a game's key. */
static void scramble(unsigned char buf[], unsigned size, const unsigned char table[8])
{
    unsigned table_ptr = 0;
    unsigned char var = 0x88;
    unsigned char tmp;

    do {
        tmp = buf[--size];
        buf[size] = tmp ^ table[table_ptr] ^ var;
        var += size + tmp;
        if (++table_ptr >= 8)
            table_ptr = 0;
    } while (size != 0);
}

static float clamp_axis(double x)
{
    return (float)(x > NP_AXIS_MAX ? NP_AXIS_MAX : x < -NP_AXIS_MAX ? -NP_AXIS_MAX : x);
}

NP_EXPORT(int) NP_GetData(tir_data_t *data)
{
    static short frame;
    memset(data, 0, sizeof *data);
    data->frame = ++frame;
    data->status = NPCLIENT_STATUS_DISABLED;

    ensure_trackkat();
    if (map_shared()) {
        InterlockedIncrement(&control->game_reads);
        register_game();

        unsigned id = heap->data.DataID;
        ULONGLONG now = GetTickCount64();
        if (id != last_data_id) {
            last_data_id = id;
            last_data_change = now;
        }
        if (id != 0 && now - last_data_change < STALE_MS) {
            data->status = NPCLIENT_STATUS_OK;
            data->yaw = clamp_axis(heap->data.Yaw * NP_AXIS_MAX / PI);
            data->pitch = clamp_axis(heap->data.Pitch * NP_AXIS_MAX / PI);
            data->roll = clamp_axis(heap->data.Roll * NP_AXIS_MAX / PI);
            data->tx = clamp_axis(heap->data.X * NP_AXIS_MAX / 500.);
            data->ty = clamp_axis(heap->data.Y * NP_AXIS_MAX / 500.);
            data->tz = clamp_axis(heap->data.Z * NP_AXIS_MAX / 500.);
        }
    }

    int status = data->status;
    static int last_status = -1;
    static unsigned calls;
    if (status != last_status || ++calls % 1200 == 0) {
        np_log("NP_GetData #%u: status %s, yaw %.0f pitch %.0f roll %.0f x %.0f y %.0f z %.0f, %s",
             calls, status == NPCLIENT_STATUS_OK ? "ok" : "disabled", data->yaw, data->pitch, data->roll,
             data->tx, data->ty, data->tz, key ? "scrambled" : "plain");
        last_status = status;
    }
    data->cksum = cksum((unsigned char *)data, sizeof *data);
    if (key)
        scramble((unsigned char *)data, sizeof *data, key);
    return status;
}

NP_EXPORT(int) NP_RegisterProgramProfileID(unsigned short id)
{
    LOG_FIRST(5, "NP_RegisterProgramProfileID(%u)", id);
    const char *name = NULL;
    for (size_t i = 0; i < sizeof scrambled_games / sizeof scrambled_games[0]; i++)
        if (scrambled_games[i].id == id) {
            key = scrambled_games[i].key;
            name = scrambled_games[i].name;
        }
    if (name)
        np_log("game %u is %s: scrambling its data", id, name);
    else
        np_log("game %u: sending plain data", id);
    if (map_shared())
        heap->GameID = id;
    ensure_trackkat();
    return 0;
}

NP_EXPORT(int) NP_ReCenter(void)
{
    LOG_FIRST(20, "NP_ReCenter");
    if (map_shared())
        InterlockedIncrement(&control->recenter_requests);
    return 0;
}

NP_EXPORT(int) NP_QueryVersion(unsigned short *version)
{
    LOG_FIRST(5, "NP_QueryVersion");
    *version = 0x0500;
    return 0;
}

/* NaturalPoint's signature, stored split so it doesn't appear in the binary
 * as text. */
static volatile unsigned char part1_1[200] = {
    0x1d, 0x79, 0xce, 0x35, 0x1d, 0x95, 0x79, 0xdf, 0x4c, 0x8d, 0x55, 0xeb, 0x20, 0x17,
    0x9f, 0x26, 0x3e, 0xf0, 0x88, 0x8e, 0x7a, 0x08, 0x11, 0x52, 0xfc, 0xd8, 0x3f, 0xb9,
    0xd2, 0x5c, 0x61, 0x03, 0x56, 0xfd, 0xbc, 0xb4, 0x0a, 0xf1, 0x13, 0x5d, 0x90, 0x0a,
    0x0e, 0xee, 0x09, 0x19, 0x45, 0x5a, 0xeb, 0xe3, 0xf0, 0x58, 0x5f, 0xac, 0x23, 0x84,
    0x1f, 0xc5, 0xe3, 0xa6, 0x18, 0x5d, 0xb8, 0x47, 0xdc, 0xe6, 0xf2, 0x0b, 0x03, 0x55,
    0x61, 0xab, 0xe3, 0x57, 0xe3, 0x67, 0xcc, 0x16, 0x38, 0x3c, 0x11, 0x25, 0x88, 0x8a,
    0x24, 0x7f, 0xf7, 0xeb, 0xf2, 0x5d, 0x82, 0x89, 0x05, 0x53, 0x32, 0x6b, 0x28, 0x54,
    0x13, 0xf6, 0xe7, 0x21, 0x1a, 0xc6, 0xe3, 0xe1, 0xff};
static volatile unsigned char part1_2[200] = {
    0x6d, 0x0b, 0xab, 0x56, 0x74, 0xe6, 0x1c, 0xff, 0x24, 0xe8, 0x34, 0x8f, 0x00, 0x63,
    0xed, 0x47, 0x5d, 0x9b, 0xe1, 0xe0, 0x1d, 0x02, 0x31, 0x22, 0x89, 0xac, 0x1f, 0xc0,
    0xbd, 0x29, 0x13, 0x23, 0x3e, 0x98, 0xdd, 0xd0, 0x2a, 0x98, 0x7d, 0x29, 0xff, 0x2a,
    0x7a, 0x86, 0x6c, 0x39, 0x22, 0x3b, 0x86, 0x86, 0xfa, 0x78, 0x31, 0xc3, 0x54, 0xa4,
    0x78, 0xaa, 0xc3, 0xca, 0x77, 0x32, 0xd3, 0x67, 0xbd, 0x94, 0x9d, 0x7e, 0x6d, 0x31,
    0x6b, 0xa1, 0xc3, 0x14, 0x8c, 0x17, 0xb5, 0x64, 0x51, 0x5b, 0x79, 0x51, 0xa8, 0xcf,
    0x5d, 0x1a, 0xb4, 0x84, 0x9c, 0x29, 0xf0, 0xe6, 0x69, 0x73, 0x66, 0x0e, 0x4b, 0x3c,
    0x7d, 0x99, 0x8b, 0x4e, 0x7d, 0xaf, 0x86, 0x92, 0xff};
static volatile unsigned char part2_1[200] = {
    0x8b, 0x84, 0xfc, 0x8c, 0x71, 0xb5, 0xd9, 0xaa, 0xda, 0x32, 0xc7, 0xe9, 0x0c, 0x20,
    0x40, 0xd4, 0x4b, 0x02, 0x89, 0xca, 0xde, 0x61, 0x9d, 0xfb, 0xb3, 0x8c, 0x97, 0x8a,
    0x13, 0x6a, 0x0f, 0xf8, 0xf8, 0x0d, 0x65, 0x1b, 0xe3, 0x05, 0x1e, 0xb6, 0xf6, 0xd9,
    0x13, 0xad, 0xeb, 0x38, 0xdd, 0x86, 0xfc, 0x59, 0x2e, 0xf6, 0x2e, 0xf4, 0xb0, 0xb0,
    0xfd, 0xb0, 0x70, 0x23, 0xfb, 0xc9, 0x1a, 0x50, 0x89, 0x92, 0xf0, 0x01, 0x09, 0xa1,
    0xfd, 0x5b, 0x19, 0x29, 0x73, 0x59, 0x2b, 0x81, 0x83, 0x9e, 0x11, 0xf3, 0xa2, 0x1f,
    0xc8, 0x24, 0x53, 0x60, 0x0a, 0x42, 0x78, 0x7a, 0x39, 0xea, 0xc1, 0x59, 0xad, 0xc5,
    0x00};
static volatile unsigned char part2_2[200] = {
    0xe3, 0xe5, 0x8e, 0xe8, 0x06, 0xd4, 0xab, 0xcf, 0xfa, 0x51, 0xa6, 0x84, 0x69, 0x52,
    0x21, 0xde, 0x6b, 0x71, 0xe6, 0xac, 0xaa, 0x16, 0xfc, 0x89, 0xd6, 0xac, 0xe7, 0xf8,
    0x7c, 0x09, 0x6a, 0x8b, 0x8b, 0x64, 0x0b, 0x7c, 0xc3, 0x61, 0x7f, 0xc2, 0x97, 0xd3,
    0x33, 0xd9, 0x99, 0x59, 0xbe, 0xed, 0xdc, 0x2c, 0x5d, 0x93, 0x5c, 0xd4, 0xdd, 0xdf,
    0x8b, 0xd5, 0x1d, 0x46, 0x95, 0xbd, 0x10, 0x5a, 0xa9, 0xd1, 0x9f, 0x71, 0x70, 0xd3,
    0x94, 0x3c, 0x71, 0x5d, 0x53, 0x1c, 0x52, 0xe4, 0xc0, 0xf1, 0x7f, 0x87, 0xd0, 0x70,
    0xa4, 0x04, 0x07, 0x05, 0x69, 0x2a, 0x16, 0x15, 0x55, 0x85, 0xa6, 0x30, 0xc8, 0xb6,
    0x00};

NP_EXPORT(int) NP_GetSignature(tir_signature_t *sig)
{
    LOG_FIRST(5, "NP_GetSignature");
    for (int i = 0; i < 200; i++)
        sig->DllSignature[i] = part1_2[i] ^ part1_1[i];
    for (int i = 0; i < 200; i++)
        sig->AppSignature[i] = part2_1[i] ^ part2_2[i];
    return 0;
}

/* The rest of TrackIR's API, which games call but we don't need. */
NP_EXPORT(int) NP_GetParameter(int a, int b) { LOG_FIRST(5, "NP_GetParameter(%d, %d)", a, b); return 0; }
NP_EXPORT(int) NP_SetParameter(int a, int b) { LOG_FIRST(5, "NP_SetParameter(%d, %d)", a, b); return 0; }
NP_EXPORT(int) NP_RegisterWindowHandle(HWND hwnd) { (void)hwnd; LOG_FIRST(5, "NP_RegisterWindowHandle"); return 0; }
NP_EXPORT(int) NP_UnregisterWindowHandle(void) { LOG_FIRST(5, "NP_UnregisterWindowHandle"); return 0; }
NP_EXPORT(int) NP_RequestData(unsigned short req) { LOG_FIRST(5, "NP_RequestData(0x%x)", req); return 0; }
NP_EXPORT(int) NP_StartCursor(void) { LOG_FIRST(5, "NP_StartCursor"); return 0; }
NP_EXPORT(int) NP_StopCursor(void) { LOG_FIRST(5, "NP_StopCursor"); return 0; }
NP_EXPORT(int) NP_StartDataTransmission(void)
{
    LOG_FIRST(5, "NP_StartDataTransmission");
    if (map_shared())
        register_game();
    return 0;
}
NP_EXPORT(int) NP_StopDataTransmission(void)
{
    LOG_FIRST(5, "NP_StopDataTransmission");
    unregister_game();
    return 0;
}
NP_EXPORT(int) NPPriv_ClientNotify(void) { return 0; }
NP_EXPORT(int) NPPriv_GetLastError(void) { return 0; }
NP_EXPORT(int) NPPriv_SetData(void) { return 0; }
NP_EXPORT(int) NPPriv_SetLastError(void) { return 0; }
NP_EXPORT(int) NPPriv_SetParameter(void) { return 0; }
NP_EXPORT(int) NPPriv_SetSignature(void) { return 0; }
NP_EXPORT(int) NPPriv_SetVersion(void) { return 0; }
