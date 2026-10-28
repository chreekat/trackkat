/* NPClient.dll / NPClient64.dll: what games load to get TrackIR data.
 *
 * It reads the pose trackkat.exe publishes in FreeTrack shared memory, and
 * starts trackkat.exe (from the DLL's own folder) if it isn't running. The
 * record games get is made as cbits/npclient_common.c says.
 *
 * Derived from OpenTrack's contrib/npclient/npclient.c (ISC licence), by
 * uglyDwarf of linuxtrack (MIT licence). */

#include <windows.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "../cbits/npclient_common.h"

#define NP_EXPORT(t) __declspec(dllexport) t __stdcall

static HMODULE self;
static HANDLE log_file = INVALID_HANDLE_VALUE;
static HANDLE heap_mapping, control_mapping;
static volatile FTHeap *heap;
static volatile TrackKatControl *control;
static const unsigned char *key;
static bool launch_attempted;
static np_freshness freshness;

/* Append a line to npclient.log, so we can see what a game asks for. */
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

/* Open npclient.log in %APPDATA%\trackkat, beside trackkat's settings, or
 * beside the DLL without an %APPDATA%. */
static void open_log(void)
{
    wchar_t path[MAX_PATH];
    DWORD n = GetEnvironmentVariableW(L"APPDATA", path, MAX_PATH);
    if (n > 0 && n + wcslen(L"\\trackkat\\npclient.log") < MAX_PATH) {
        wcscat(path, L"\\trackkat");
        CreateDirectoryW(path, NULL);
        wcscat(path, L"\\npclient.log");
    } else {
        n = GetModuleFileNameW(self, path, MAX_PATH);
        wchar_t *slash = n && n < MAX_PATH ? wcsrchr(path, L'\\') : NULL;
        if (!slash || (size_t)(slash + 1 - path) + wcslen(L"npclient.log") >= MAX_PATH)
            return;
        wcscpy(slash + 1, L"npclient.log");
    }
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
 * done with it. Tried once per process. An installed trackkat.exe whose
 * version is no longer the newest hands over to the newest. */
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
    registered = np_add_game(control, (long)GetCurrentProcessId());
    if (!registered)
        LOG_FIRST(1, "no free slot to register this game in");
}

static void unregister_game(void)
{
    if (!registered || !control)
        return;
    np_remove_game(control, (long)GetCurrentProcessId());
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

NP_EXPORT(int) NP_GetData(tir_data_t *data)
{
    static short frame;
    memset(data, 0, sizeof *data);
    data->frame = ++frame;
    data->status = NP_STATUS_DISABLED;

    ensure_trackkat();
    if (map_shared()) {
        InterlockedIncrement(&control->game_reads);
        register_game();
        np_fill_record(data, &heap->data, np_pose_fresh(&freshness, heap->data.DataID, GetTickCount64()));
    }

    int status = data->status;
    static int last_status = -1;
    static unsigned calls;
    if (status != last_status || ++calls % 1200 == 0) {
        np_log("NP_GetData #%u: status %s, yaw %.0f pitch %.0f roll %.0f x %.0f y %.0f z %.0f, %s",
             calls, status == NP_STATUS_OK ? "ok" : "disabled", data->yaw, data->pitch, data->roll,
             data->tx, data->ty, data->tz, key ? "scrambled" : "plain");
        last_status = status;
    }
    np_seal_record(data, key);
    return status;
}

NP_EXPORT(int) NP_RegisterProgramProfileID(unsigned short id)
{
    LOG_FIRST(5, "NP_RegisterProgramProfileID(%u)", id);
    const char *name;
    key = np_game_key(id, &name);
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

NP_EXPORT(int) NP_GetSignature(tir_signature_t *sig)
{
    LOG_FIRST(5, "NP_GetSignature");
    np_signature(sig);
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
