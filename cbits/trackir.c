/* trackkat.exe standing in for NaturalPoint's TrackIR software, so that games
 * can load NaturalPoint's own NPClient DLL, which is signed, where anti-cheat
 * refuses ours.
 *
 * NaturalPoint's DLL finds its software by a top-level window titled
 * "NaturalPoint", and sends it message 0x405 with a command and its argument;
 * the software answers in shared memory. The DLL hands games the pose record
 * it finds in that shared memory, copied under NPClientMutex, as is: the
 * software checksums it and scrambles it with the game's key. Here the window
 * has a thread of its own, answering at once, and the record is written on
 * every frame, as cbits/npclient_common.c makes it for our own DLL.
 *
 * The DLL registers a game only once, when it starts head tracking, so
 * trackkat remembers the game it serves (hts_trackir_served), and a trackkat
 * started while that game still runs takes it up again (hts_trackir_start):
 * its key, its place among the games tracking, and the record's frame
 * count. */

#include <windows.h>
#include <sddl.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <tlhelp32.h>

#include "npclient_common.h"
#include "trackir.h"

#define TRACKIR_SHARED "Local\\SharedTrackIRData"
#define TRACKIR_MUTEX "NPClientMutex"
#define TRACKIR_WINDOW L"NaturalPoint"
#define TRACKIR_MESSAGE 0x405

/* The commands, as the message's wParam. */
enum {
    CMD_REGISTER_PROFILE = 1010,  /* lParam: the game's profile ID */
    CMD_REGISTER_WINDOW = 1020,   /* lParam: the game's window */
    CMD_UNREGISTER_WINDOW = 1021,
    CMD_START_DATA = 2010,
    CMD_STOP_DATA = 2020,
    CMD_RECENTER = 3030,
};

/* The shared memory, as NaturalPoint's DLL lays it out. */
#pragma pack(push, 1)
typedef struct TrackIRShared {
    uint16_t version;    /* NP_QueryVersion */
    uint32_t result;     /* of the last command */
    uint32_t params[3];
    tir_signature_t signature; /* NP_GetSignature */
    tir_data_t data;     /* NP_GetData */
} TrackIRShared;
#pragma pack(pop)

_Static_assert(offsetof(TrackIRShared, signature) == 0x12, "TrackIR's signature");
_Static_assert(offsetof(TrackIRShared, data) == 0x1A2, "TrackIR's pose record");
_Static_assert(sizeof(TrackIRShared) == 0x1E6, "TrackIR's shared memory");

static HANDLE mapping, mutex;
static volatile TrackIRShared *shared;
/* The last pose published, for the stale check on the window's thread. */
static const volatile FTData *latest;

/* Guarded by the mutex. */
static np_freshness freshness;
static short frame;
static int written_status = -1;

/* Set on the window's thread, read when writing the record. */
static const unsigned char *volatile key;
/* The profile ID a game last registered, until hts_trackir_take_game. */
static volatile LONG registered_profile = -1;

/* Only the window's thread touches these, once it has started. */
static DWORD game_pid;
static ULONGLONG game_created; /* when the game's process started; 0 if unknown */
static HWND game_window;
static int game_profile = -1;
static int game_registered;

/* The game served, as hts_trackir_served reports it: all zero for none. */
typedef struct served_game {
    DWORD pid;
    ULONGLONG created;
    ULONGLONG window;
    int profile;
    int tracking;
} served_game;

/* Guarded by served_lock. */
static CRITICAL_SECTION served_lock;
static served_game served;
static long served_changes;

/* When to write the record. */
enum when { ALWAYS, NEWLY_STALE };

/* Write the pose record, unless the mutex stays busy for longer than wait
 * ms. NEWLY_STALE writes it only to say that trackkat has stopped
 * publishing. */
static void write_record(const volatile FTData *pose, DWORD wait, enum when when)
{
    DWORD w = WaitForSingleObject(mutex, wait);
    if (w != WAIT_OBJECT_0 && w != WAIT_ABANDONED)
        return;
    int fresh = pose && np_pose_fresh(&freshness, pose->DataID, GetTickCount64());
    if (when == ALWAYS || (!fresh && written_status != NP_STATUS_DISABLED)) {
        tir_data_t d;
        memset(&d, 0, sizeof d);
        d.frame = ++frame;
        np_fill_record(&d, pose, fresh);
        written_status = d.status;
        np_seal_record(&d, key);
        memcpy((void *)&shared->data, &d, sizeof d);
    }
    ReleaseMutex(mutex);
}

void trackir_publish(const volatile FTData *pose)
{
    if (!shared)
        return;
    latest = pose;
    write_record(pose, 0, ALWAYS);
}

static void game_tracking(int tracking)
{
    volatile TrackKatControl *control = hts_control_block();
    if (!game_pid || !control || tracking == game_registered)
        return;
    if (tracking)
        game_registered = np_add_game(control, (long)game_pid);
    else {
        np_remove_game(control, (long)game_pid);
        game_registered = 0;
    }
}

/* Whether a process is running: 0 if it has exited, or never was; 1 if it
 * is, and *created says when it started; 2 if it is, but Windows won't say
 * when it started, as anti-cheat may have it. */
int hts_process_started(unsigned long pid, unsigned long long *created)
{
    *created = 0;
    HANDLE p = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, (DWORD)pid);
    if (!p)
        return GetLastError() == ERROR_ACCESS_DENIED ? 2 : 0;
    DWORD code = 0;
    FILETIME start, ended, kernel, user;
    int state = !GetExitCodeProcess(p, &code) || code != STILL_ACTIVE ? 0
                : GetProcessTimes(p, &start, &ended, &kernel, &user) ? 1
                : 2;
    if (state == 1)
        *created = (ULONGLONG)start.dwHighDateTime << 32 | start.dwLowDateTime;
    CloseHandle(p);
    return state;
}

/* The process a window belongs to, or 0 if there is no such window. */
unsigned long hts_window_process(unsigned long long window)
{
    DWORD pid = 0;
    if (window && IsWindow((HWND)(uintptr_t)window))
        GetWindowThreadProcessId((HWND)(uintptr_t)window, &pid);
    return pid;
}

/* Report the game served as it now stands, counting the changes. */
static void note_served(void)
{
    served_game now = {0, 0, 0, 0, 0};
    if (game_pid) {
        now.pid = game_pid;
        now.created = game_created;
        now.window = (ULONGLONG)(uintptr_t)game_window;
        now.profile = game_profile;
        now.tracking = game_registered;
    }
    EnterCriticalSection(&served_lock);
    if (now.pid != served.pid || now.created != served.created || now.window != served.window
        || now.profile != served.profile || now.tracking != served.tracking) {
        served = now;
        served_changes++;
    }
    LeaveCriticalSection(&served_lock);
}

/* Forget a game once its process has exited. */
static void forget_exited_game(void)
{
    ULONGLONG created;
    if (!game_pid)
        return;
    int state = hts_process_started(game_pid, &created);
    if (state == 2 || (state == 1 && (!game_created || created == game_created)))
        return;
    game_tracking(0);
    game_pid = 0;
    game_created = 0;
    game_window = NULL;
    note_served();
}

static void command(WPARAM code, LPARAM arg)
{
    switch (code) {
    case CMD_REGISTER_PROFILE:
        key = np_game_key((unsigned short)arg, NULL);
        /* The game's first NP_GetData mustn't find a record made for the
         * last game. */
        write_record(latest, 25, ALWAYS);
        game_profile = (unsigned short)arg;
        InterlockedExchange(&registered_profile, (LONG)(unsigned short)arg);
        break;
    case CMD_REGISTER_WINDOW: {
        DWORD pid = 0;
        GetWindowThreadProcessId((HWND)arg, &pid);
        if (pid != game_pid) {
            game_tracking(0);
            game_pid = pid;
            hts_process_started(pid, &game_created);
        }
        game_window = (HWND)arg;
        game_tracking(1);
        break;
    }
    case CMD_UNREGISTER_WINDOW:
        game_tracking(0);
        game_pid = 0;
        game_created = 0;
        game_window = NULL;
        break;
    case CMD_START_DATA:
        game_tracking(1);
        break;
    case CMD_STOP_DATA:
        game_tracking(0);
        break;
    case CMD_RECENTER: {
        volatile TrackKatControl *control = hts_control_block();
        if (control)
            InterlockedIncrement(&control->recenter_requests);
        break;
    }
    default:
        break;
    }
    note_served();
    shared->result = 0;
}

static LRESULT CALLBACK window_proc(HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
    switch (msg) {
    case TRACKIR_MESSAGE:
        command(wparam, lparam);
        return 0;
    case WM_TIMER:
        write_record(latest, 25, NEWLY_STALE);
        forget_exited_game();
        return 0;
    default:
        return DefWindowProcW(hwnd, msg, wparam, lparam);
    }
}

static HANDLE window_ready;
static DWORD window_error;

static DWORD WINAPI window_thread(LPVOID unused)
{
    (void)unused;
    HINSTANCE instance = GetModuleHandleW(NULL);
    WNDCLASSW wc;
    memset(&wc, 0, sizeof wc);
    wc.lpfnWndProc = window_proc;
    wc.hInstance = instance;
    wc.lpszClassName = L"TrackKatTrackIR";
    /* Hidden, but top-level, not message-only: FindWindow doesn't see those. */
    HWND hwnd = RegisterClassW(&wc)
                    ? CreateWindowExW(0, wc.lpszClassName, TRACKIR_WINDOW, WS_POPUP, 0, 0, 0, 0, NULL, NULL,
                                      instance, NULL)
                    : NULL;
    window_error = hwnd ? 0 : GetLastError();
    if (hwnd) {
        /* Games may run at a lower integrity level than trackkat. */
        ChangeWindowMessageFilterEx(hwnd, TRACKIR_MESSAGE, MSGFLT_ALLOW, NULL);
        SetTimer(hwnd, 1, 250, NULL);
    }
    SetEvent(window_ready);
    if (!hwnd)
        return 0;
    MSG msg;
    while (GetMessageW(&msg, NULL, 0, 0) > 0)
        DispatchMessageW(&msg);
    return 0;
}

/* Whether NaturalPoint's software is running, so serving its DLL. */
static int trackir_software_running(void)
{
    if (FindWindowW(NULL, TRACKIR_WINDOW))
        return 1;
    HANDLE snapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if (snapshot == INVALID_HANDLE_VALUE)
        return 0;
    PROCESSENTRY32W p;
    p.dwSize = sizeof p;
    int found = 0;
    for (BOOL more = Process32FirstW(snapshot, &p); more && !found; more = Process32NextW(snapshot, &p))
        found = CompareStringOrdinal(p.szExeFile, -1, L"TrackIR5.exe", -1, TRUE) == CSTR_EQUAL;
    CloseHandle(snapshot);
    return found;
}

/* Carry on the frame count from the record a trackkat that ran before
 * left in the shared memory, which games hold open, if it checks out with
 * the key now in use. */
static void continue_frames(volatile TrackIRShared *s)
{
    DWORD w = WaitForSingleObject(mutex, 25);
    if (w != WAIT_OBJECT_0 && w != WAIT_ABANDONED)
        return;
    tir_data_t d;
    memcpy(&d, (const void *)&s->data, sizeof d);
    if (np_unseal_record(&d, key))
        frame = d.frame;
    ReleaseMutex(mutex);
}

/* Start standing in for NaturalPoint's software, serving again the game
 * that a trackkat that ran before was serving, if pid is nonzero: the
 * process, when it started (0 if unknown), its window, the profile ID it
 * registered (-1 for none) and whether it is tracking. Returns 0; -1 if
 * NaturalPoint's own software is running, which then serves its DLL; or a
 * Windows error code. */
int hts_trackir_start(unsigned long pid, unsigned long long created, unsigned long long window, int profile,
                      int tracking)
{
    if (trackir_software_running())
        return -1;

    /* As NaturalPoint's DLL makes them: anyone may use them, even a game
     * running at low integrity. */
    SECURITY_DESCRIPTOR sd;
    SECURITY_ATTRIBUTES sa = {sizeof sa, &sd, FALSE};
    PSECURITY_DESCRIPTOR low = NULL;
    InitializeSecurityDescriptor(&sd, SECURITY_DESCRIPTOR_REVISION);
    SetSecurityDescriptorDacl(&sd, TRUE, NULL, FALSE);
    PACL sacl;
    BOOL present, defaulted;
    if (ConvertStringSecurityDescriptorToSecurityDescriptorW(L"S:(ML;;NW;;;LW)", SDDL_REVISION_1, &low, NULL)
        && GetSecurityDescriptorSacl(low, &present, &sacl, &defaulted) && present)
        SetSecurityDescriptorSacl(&sd, TRUE, sacl, FALSE);

    mutex = CreateMutexA(&sa, FALSE, TRACKIR_MUTEX);
    /* A game that loaded the DLL first has made it already. */
    mapping = CreateFileMappingA(INVALID_HANDLE_VALUE, &sa, PAGE_READWRITE, 0, sizeof(TrackIRShared),
                                 TRACKIR_SHARED);
    DWORD err = GetLastError();
    if (low)
        LocalFree(low);
    if (!mutex || !mapping)
        return (int)err;
    volatile TrackIRShared *s = MapViewOfFile(mapping, FILE_MAP_WRITE, 0, 0, sizeof(TrackIRShared));
    if (!s)
        return (int)GetLastError();

    s->version = 0x0500; /* as our DLL's NP_QueryVersion */
    tir_signature_t sig;
    np_signature(&sig);
    memcpy((void *)&s->signature, &sig, sizeof sig);
    if (profile >= 0) {
        key = np_game_key((unsigned short)profile, NULL);
        game_profile = profile;
    }
    continue_frames(s);
    InitializeCriticalSection(&served_lock);
    shared = s;
    write_record(NULL, 25, ALWAYS);

    if (pid) {
        game_pid = (DWORD)pid;
        game_created = created;
        game_window = (HWND)(uintptr_t)window;
        /* The control block outlives trackkat while a game using our DLL
         * holds it, perhaps with this game in it still. */
        volatile TrackKatControl *control = hts_control_block();
        if (control)
            np_remove_game(control, (long)pid);
        game_tracking(tracking);
    }
    note_served();

    window_ready = CreateEventW(NULL, TRUE, FALSE, NULL);
    HANDLE t = window_ready ? CreateThread(NULL, 0, window_thread, NULL, 0, NULL) : NULL;
    if (!t)
        return (int)GetLastError();
    CloseHandle(t);
    WaitForSingleObject(window_ready, INFINITE);
    return (int)window_error;
}

/* The profile ID a game has registered since the last call, or -1. */
int hts_trackir_take_game(void)
{
    return (int)InterlockedExchange(&registered_profile, -1);
}

/* The game served, as the window's thread last left it, in the form
 * hts_trackir_start takes it; *pid is 0 for none. Returns a count of the
 * changes to it so far. */
long hts_trackir_served(unsigned long *pid, unsigned long long *created, unsigned long long *window, int *profile,
                        int *tracking)
{
    if (!shared) {
        *pid = 0;
        return 0;
    }
    EnterCriticalSection(&served_lock);
    *pid = served.pid;
    *created = served.created;
    *window = served.window;
    *profile = served.profile;
    *tracking = served.tracking;
    long changes = served_changes;
    LeaveCriticalSection(&served_lock);
    return changes;
}

/* The name of a game whose data is scrambled, or NULL. */
const char *hts_scrambled_game(int id)
{
    const char *name;
    np_game_key((unsigned short)id, &name);
    return name;
}
