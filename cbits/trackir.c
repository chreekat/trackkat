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
 * every frame, as cbits/npclient_common.c makes it for our own DLL. */

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

/* Only the window's thread touches these. */
static DWORD game_pid;
static int game_registered;

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

static void command(WPARAM code, LPARAM arg)
{
    switch (code) {
    case CMD_REGISTER_PROFILE:
        key = np_game_key((unsigned short)arg, NULL);
        /* The game's first NP_GetData mustn't find a record made for the
         * last game. */
        write_record(latest, 25, ALWAYS);
        InterlockedExchange(&registered_profile, (LONG)(unsigned short)arg);
        break;
    case CMD_REGISTER_WINDOW: {
        DWORD pid = 0;
        GetWindowThreadProcessId((HWND)arg, &pid);
        if (pid != game_pid) {
            game_tracking(0);
            game_pid = pid;
        }
        game_tracking(1);
        break;
    }
    case CMD_UNREGISTER_WINDOW:
        game_tracking(0);
        game_pid = 0;
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

/* Start standing in for NaturalPoint's software. Returns 0; -1 if its own
 * software is running, which then serves its DLL; or a Windows error
 * code. */
int hts_trackir_start(void)
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
    shared = s;
    write_record(NULL, 25, ALWAYS);

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

/* The name of a game whose data is scrambled, or NULL. */
const char *hts_scrambled_game(int id)
{
    const char *name;
    np_game_key((unsigned short)id, &name);
    return name;
}
