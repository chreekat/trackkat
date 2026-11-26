/* Windows plumbing for trackkat.exe: publishing poses to games, the control
 * block shared with our NPClient DLL, global hotkeys and joystick buttons,
 * the registry, and where what it prints goes. */

#include <windows.h>
#include <io.h>
#include <stdint.h>
#include <string.h>

#include "joystick.h"
#include "npclient_common.h"
#include "trackir.h"
#include "trackkat_shared.h"

/* ---- FreeTrack shared memory ------------------------------------------- */

static HANDLE ft_mapping, ft_mutex;
static volatile FTHeap *ft_heap;
static uint32_t poses_published; /* see np_write_pose */
static HANDLE control_mapping;
static volatile TrackKatControl *control;

/* Create (or open) the FreeTrack heap, its mutex, which FreeTrack clients
 * may expect to find while trackkat runs, and our control block. Returns 0
 * on success. */
int hts_shared_open(void)
{
    ft_mutex = CreateMutexA(NULL, FALSE, FREETRACK_MUTEX);

    ft_mapping = CreateFileMappingA(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0,
                                    sizeof(FTHeap), FREETRACK_HEAP);
    if (!ft_mapping)
        return (int)GetLastError();
    ft_heap = MapViewOfFile(ft_mapping, FILE_MAP_WRITE, 0, 0, sizeof(FTHeap));
    if (!ft_heap)
        return (int)GetLastError();
    poses_published = np_pose_count(&ft_heap->data);

    control_mapping = CreateFileMappingA(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0,
                                         sizeof(TrackKatControl), TRACKKAT_CONTROL);
    if (!control_mapping)
        return (int)GetLastError();
    control = MapViewOfFile(control_mapping, FILE_MAP_WRITE, 0, 0, sizeof(TrackKatControl));
    if (!control)
        return (int)GetLastError();
    return 0;
}

/* Publish a pose: angles in radians, positions in mm, in FreeTrack's
 * conventions. FreeTrack clients that copy the pose under FT_Mutext copy it
 * whole when the mutex is free to hold while writing; if a client holds it
 * then, the pose is written all the same, never waited on. */
void hts_publish(float yaw, float pitch, float roll, float x, float y, float z)
{
    DWORD w = ft_mutex ? WaitForSingleObject(ft_mutex, 0) : WAIT_FAILED;
    np_pose pose = {0, yaw, pitch, roll, x, y, z};
    np_write_pose(&ft_heap->data, &poses_published, &pose);
    if (w == WAIT_OBJECT_0 || w == WAIT_ABANDONED)
        ReleaseMutex(ft_mutex);
    trackir_publish(&ft_heap->data);
}

volatile TrackKatControl *hts_control_block(void)
{
    return control;
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
        /* Anti-cheat may refuse even this to a running game. */
        int alive = p ? GetExitCodeProcess(p, &code) && code == STILL_ACTIVE : GetLastError() == ERROR_ACCESS_DENIED;
        if (p)
            CloseHandle(p);
        if (alive)
            n++;
        else
            InterlockedCompareExchange(&control->games[i], 0, pid);
    }
    return n;
}

/* The name of a game tracking, as its program is named without ".exe",
 * into buf; 0, leaving buf empty, if no game's program can be read. */
int hts_tracking_game_name(wchar_t *buf, int size)
{
    buf[0] = 0;
    for (int i = 0; i < TRACKKAT_MAX_GAMES; i++) {
        long pid = InterlockedCompareExchange(&control->games[i], 0, 0);
        if (pid == 0)
            continue;
        /* Anti-cheat may refuse this. */
        HANDLE p = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, (DWORD)pid);
        if (!p)
            continue;
        wchar_t path[MAX_PATH];
        DWORD n = MAX_PATH;
        BOOL ok = QueryFullProcessImageNameW(p, 0, path, &n);
        CloseHandle(p);
        if (!ok)
            continue;
        wchar_t *name = wcsrchr(path, L'\\');
        name = name ? name + 1 : path;
        size_t len = wcslen(name);
        if (len > 4 && _wcsicmp(name + len - 4, L".exe") == 0)
            name[len - 4] = 0;
        if (!name[0])
            continue;
        lstrcpynW(buf, name, size);
        return 1;
    }
    return 0;
}

/* ---- Single instance ---------------------------------------------------- */

/* Nonzero if this process now holds the trackkat instance mutex, which our
 * NPClient DLL checks to see whether trackkat is running. Otherwise it
 * keeps no handle to it, so it can try again once the holder has quit. */
int hts_single_instance(void)
{
    HANDLE m = CreateMutexA(NULL, TRUE, TRACKKAT_RUNNING_MUTEX);
    if (m && GetLastError() == ERROR_ALREADY_EXISTS) {
        CloseHandle(m);
        return 0;
    }
    return m != NULL;
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

/* Whether dir, ending in a backslash, holds the 64-bit NPClient. */
static int has_npclient(const wchar_t *dir)
{
    wchar_t path[MAX_PATH];
    if (wcslen(dir) + wcslen(L"NPClient64.dll") >= MAX_PATH)
        return 0;
    wcscpy(path, dir);
    wcscat(path, L"NPClient64.dll");
    DWORD attributes = GetFileAttributesW(path);
    return attributes != INVALID_FILE_ATTRIBUTES && !(attributes & FILE_ATTRIBUTE_DIRECTORY);
}

/* Read a folder from the registry into a buffer of len characters, ending
 * it with a backslash. */
static int read_folder(HKEY key, const wchar_t *subkey, const wchar_t *value, wchar_t *out, DWORD len)
{
    DWORD size = (len - 1) * sizeof(wchar_t);
    if (RegGetValueW(key, subkey, value, RRF_RT_REG_SZ, NULL, out, &size) != ERROR_SUCCESS)
        return 0;
    size_t n = wcslen(out);
    if (n == 0)
        return 0;
    if (out[n - 1] != L'\\') {
        out[n] = L'\\';
        out[n + 1] = 0;
    }
    return 1;
}

/* Find NaturalPoint's TrackIR software's folder, ending in a backslash, by
 * its uninstall entry, or else where it installs by default; out holds len
 * characters. Returns 0 unless the folder has NPClient64.dll in it. */
int hts_find_trackir_dir(wchar_t *out, int len)
{
    static const REGSAM views[] = {KEY_WOW64_32KEY, KEY_WOW64_64KEY};
    for (int v = 0; v < 2; v++) {
        HKEY key;
        if (RegOpenKeyExW(HKEY_LOCAL_MACHINE, L"Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall", 0,
                          KEY_READ | views[v], &key) != ERROR_SUCCESS)
            continue;
        wchar_t name[256], display[256];
        int found = 0;
        for (DWORD i = 0; !found; i++) {
            DWORD n = 256;
            if (RegEnumKeyExW(key, i, name, &n, NULL, NULL, NULL, NULL) != ERROR_SUCCESS)
                break;
            DWORD size = sizeof display;
            found = RegGetValueW(key, name, L"DisplayName", RRF_RT_REG_SZ, NULL, display, &size) == ERROR_SUCCESS
                    && _wcsnicmp(display, L"TrackIR", 7) == 0
                    && read_folder(key, name, L"InstallLocation", out, (DWORD)len) && has_npclient(out);
        }
        RegCloseKey(key);
        if (found)
            return 1;
    }
    DWORD n = GetEnvironmentVariableW(L"ProgramFiles(x86)", out, (DWORD)len);
    if (n == 0 || n >= (DWORD)len)
        n = GetEnvironmentVariableW(L"ProgramFiles", out, (DWORD)len);
    if (n == 0 || n + wcslen(L"\\TrackIR5\\") >= (DWORD)len)
        return 0;
    wcscat(out, L"\\TrackIR5\\");
    return has_npclient(out);
}

/* ---- Hotkeys and joystick buttons ---------------------------------------- */

/* Keys are watched with a low-level keyboard hook rather than RegisterHotKey,
 * which refuses F12 (reserved for debuggers) and swallows the key, hiding it
 * from games bound to the same key. The middle and side mouse buttons are
 * watched with a low-level mouse hook, as keys with their virtual-key codes,
 * and likewise never swallowed. Joysticks (joystick.c) are watched on the
 * same thread.
 *
 * All that is told of the watched keys and buttons is each going down and
 * coming up, and when: what a press does is decided by whoever takes the
 * edges. */

/* Modifier flags, as RegisterHotKey numbers them. */
#define HK_ALT 0x1
#define HK_CTRL 0x2
#define HK_SHIFT 0x4
#define HK_WIN 0x8

#define MAX_WATCHED 64
#define MAX_EDGES 64

#define EDGE_DOWN 1
#define EDGE_UP 2

/* A key or joystick button watched. */
typedef struct {
    LONG key; /* mods << 16 | vk, or 0 for a joystick button */
    wchar_t joystick[JOYSTICK_NAME_LEN];
    int index; /* which of the joysticks with that name, from 1 */
    int button;
} Watched;

static SRWLOCK watched_lock = SRWLOCK_INIT;
static Watched watched[MAX_WATCHED];
static int watched_count;
/* Keys and buttons being gathered by hts_bind_*, until hts_bindings_apply. */
static Watched staged[MAX_WATCHED];
static int staged_count;

/* A watched key or button going down or coming up. */
typedef struct {
    Watched what;
    int edge;    /* EDGE_DOWN or EDGE_UP */
    LONGLONG at; /* QueryPerformanceCounter's count then */
} Edge;

/* Edges not yet taken, oldest first; new ones are dropped while it is
 * full. */
static SRWLOCK edges_lock = SRWLOCK_INIT;
static Edge edges[MAX_EDGES];
static int edges_first, edges_count;

static volatile LONG hotkey_status; /* 0 starting, 1 started */
static DWORD keyboard_error, mouse_error, joystick_error;

/* Start gathering a new set of keys and buttons to watch; the old ones stay
 * watched until hts_bindings_apply. Those past MAX_WATCHED are ignored. */
void hts_bindings_clear(void)
{
    staged_count = 0;
}

void hts_bind_key(unsigned mods, unsigned vk)
{
    if (staged_count == MAX_WATCHED)
        return;
    Watched *w = &staged[staged_count++];
    w->key = (LONG)(mods << 16 | vk);
    w->joystick[0] = 0;
    w->index = 0;
    w->button = 0;
}

void hts_bind_button(const wchar_t *joystick, int index, int button)
{
    if (staged_count == MAX_WATCHED)
        return;
    Watched *w = &staged[staged_count++];
    w->key = 0;
    lstrcpynW(w->joystick, joystick, JOYSTICK_NAME_LEN);
    w->index = index;
    w->button = button;
}

/* Watch the gathered keys and buttons, whether or not the keyboard and
 * joysticks are being watched yet. */
void hts_bindings_apply(void)
{
    AcquireSRWLockExclusive(&watched_lock);
    memcpy(watched, staged, sizeof staged);
    watched_count = staged_count;
    ReleaseSRWLockExclusive(&watched_lock);
}

/* The modifiers held now, as RegisterHotKey's flags. Also used by the
 * window's key boxes (cbits/gui.c). */
unsigned hts_modifiers_held(void)
{
    unsigned m = 0;
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

static void note_edge(const Watched *what, int edge)
{
    LARGE_INTEGER now;
    QueryPerformanceCounter(&now);
    AcquireSRWLockExclusive(&edges_lock);
    if (edges_count < MAX_EDGES) {
        Edge *e = &edges[(edges_first + edges_count++) % MAX_EDGES];
        e->what = *what;
        e->edge = edge;
        e->at = now.QuadPart;
    }
    ReleaseSRWLockExclusive(&edges_lock);
}

/* The oldest edge of a watched key or button not yet taken: EDGE_DOWN or
 * EDGE_UP, or 0 if there is none. A key is told as *key, mods << 16 | vk;
 * a joystick button, with *key 0, as the joystick's name as it was bound,
 * which of the joysticks with that name it is, and the button. *age is how
 * many seconds ago it happened. */
int hts_key_edge(int *key, wchar_t *joystick, int joystick_len, int *index, int *button, double *age)
{
    LARGE_INTEGER now, frequency;
    QueryPerformanceCounter(&now);
    QueryPerformanceFrequency(&frequency);
    int edge = 0;
    AcquireSRWLockExclusive(&edges_lock);
    if (edges_count > 0) {
        const Edge *e = &edges[edges_first];
        edge = e->edge;
        *key = (int)e->what.key;
        lstrcpynW(joystick, e->what.joystick, joystick_len);
        *index = e->what.index;
        *button = e->what.button;
        *age = (double)(now.QuadPart - e->at) / (double)frequency.QuadPart;
        edges_first = (edges_first + 1) % MAX_EDGES;
        edges_count--;
    }
    ReleaseSRWLockExclusive(&edges_lock);
    return edge;
}

/* Windows doesn't say which key-downs are a held key repeating. A key
 * repeats at least every second or so, so a key-down longer than this after
 * the last one, with no key-up between, is a new press whose key-up before
 * went unseen, as when the screen was locked meanwhile. */
#define REPEAT_WITHIN_MS 2000

/* The watched keys and mouse buttons now down, by virtual-key code. Only the
 * hotkey thread touches it. */
static struct {
    LONG key;   /* what it went down as, mods << 16 | vk; 0 while it is up */
    DWORD last; /* GetTickCount at its latest key-down */
} held_keys[256];

/* A key, or mouse button, comes up: told if it went down as a watched
 * key, whatever modifiers are held now. */
static void key_up(DWORD vk)
{
    if (vk >= ARRAYSIZE(held_keys) || !held_keys[vk].key)
        return;
    Watched w = {.key = held_keys[vk].key};
    held_keys[vk].key = 0;
    note_edge(&w, EDGE_UP);
}

/* A key, or mouse button, goes down: told if it is watched with just the
 * modifiers held now, and isn't a held key repeating. */
static void key_down(DWORD vk)
{
    if (vk >= ARRAYSIZE(held_keys))
        return;
    DWORD now = GetTickCount();
    if (held_keys[vk].key) {
        if (now - held_keys[vk].last < REPEAT_WITHIN_MS) {
            held_keys[vk].last = now;
            return;
        }
        key_up(vk);
    }
    Watched w = {.key = (LONG)(hts_modifiers_held() << 16 | vk)};
    int found = 0;
    AcquireSRWLockShared(&watched_lock);
    for (int i = 0; i < watched_count && !found; i++)
        found = watched[i].key == w.key;
    ReleaseSRWLockShared(&watched_lock);
    if (!found)
        return;
    held_keys[vk].key = w.key;
    held_keys[vk].last = now;
    note_edge(&w, EDGE_DOWN);
}

static LRESULT CALLBACK keyboard_hook(int code, WPARAM wparam, LPARAM lparam)
{
    if (code == HC_ACTION) {
        const KBDLLHOOKSTRUCT *k = (const KBDLLHOOKSTRUCT *)lparam;
        if (wparam == WM_KEYDOWN || wparam == WM_SYSKEYDOWN)
            key_down(k->vkCode);
        else if (wparam == WM_KEYUP || wparam == WM_SYSKEYUP)
            key_up(k->vkCode);
    }
    return CallNextHookEx(NULL, code, wparam, lparam);
}

static LRESULT CALLBACK mouse_hook(int code, WPARAM wparam, LPARAM lparam)
{
    if (code == HC_ACTION) {
        const MSLLHOOKSTRUCT *m = (const MSLLHOOKSTRUCT *)lparam;
        DWORD side = HIWORD(m->mouseData) == XBUTTON1 ? VK_XBUTTON1 : VK_XBUTTON2;
        if (wparam == WM_MBUTTONDOWN)
            key_down(VK_MBUTTON);
        else if (wparam == WM_MBUTTONUP)
            key_up(VK_MBUTTON);
        else if (wparam == WM_XBUTTONDOWN)
            key_down(side);
        else if (wparam == WM_XBUTTONUP)
            key_up(side);
    }
    return CallNextHookEx(NULL, code, wparam, lparam);
}

void watched_button(const wchar_t *joystick, int index, int button, int down)
{
    Watched matched[MAX_WATCHED];
    int n = 0;
    AcquireSRWLockShared(&watched_lock);
    for (int i = 0; i < watched_count; i++) {
        const Watched *w = &watched[i];
        if (w->key == 0 && w->button == button && w->index == index
            && CompareStringOrdinal(w->joystick, -1, joystick, -1, TRUE) == CSTR_EQUAL)
            matched[n++] = *w;
    }
    ReleaseSRWLockShared(&watched_lock);
    for (int i = 0; i < n; i++)
        note_edge(&matched[i], down ? EDGE_DOWN : EDGE_UP);
}

static DWORD WINAPI hotkey_thread(LPVOID unused)
{
    (void)unused;
    joystick_error = joysticks_watch();
    /* Hooked last, with nothing between them and the message loop: Windows
     * drops a low-level hook whose thread is slow to answer it. */
    HHOOK hook = SetWindowsHookExW(WH_KEYBOARD_LL, keyboard_hook, GetModuleHandleW(NULL), 0);
    keyboard_error = hook ? 0 : GetLastError();
    HHOOK mouse = SetWindowsHookExW(WH_MOUSE_LL, mouse_hook, GetModuleHandleW(NULL), 0);
    mouse_error = mouse ? 0 : GetLastError();
    InterlockedExchange(&hotkey_status, 1);
    if (keyboard_error && mouse_error && joystick_error)
        return 0;

    /* The hook and the joystick window run only while this thread pumps
     * messages. */
    MSG msg;
    while (GetMessageW(&msg, NULL, 0, 0) > 0)
        DispatchMessageW(&msg);
    if (hook)
        UnhookWindowsHookEx(hook);
    if (mouse)
        UnhookWindowsHookEx(mouse);
    return 0;
}

/* Start watching the keyboard, mouse and joysticks for the watched keys and
 * buttons on a thread of their own. Returns 0, or a Windows error code if
 * the keyboard can't be watched; sets *mouse_err and *joysticks_err
 * likewise. */
int hts_hotkeys_start(int *mouse_err, int *joysticks_err)
{
    HANDLE t = CreateThread(NULL, 0, hotkey_thread, NULL, 0, NULL);
    if (!t) {
        *joysticks_err = *mouse_err = (int)GetLastError();
        return *joysticks_err;
    }
    CloseHandle(t);
    while (InterlockedCompareExchange(&hotkey_status, 0, 0) == 0)
        Sleep(1);
    *mouse_err = (int)mouse_error;
    *joysticks_err = (int)joystick_error;
    return (int)keyboard_error;
}

/* ---- Console ------------------------------------------------------------- */

/* trackkat.exe is a GUI program, so Windows gives it no console, and its
 * standard output and error are whatever it was started with, which may be
 * nothing. Where they now go, as attach_console found: */
#define OUTPUT_REDIRECTED 0 /* the file or pipe it was started with */
#define OUTPUT_PARENT_CONSOLE 1 /* the console of the terminal that started it */
#define OUTPUT_NONE 2 /* NUL, or for a console program, its own console */

static int console_output = OUTPUT_NONE;

/* A file or pipe to write to, as given by whatever started this process. */
static int redirected(DWORD std)
{
    HANDLE h = GetStdHandle(std);
    if (h == NULL || h == INVALID_HANDLE_VALUE)
        return 0;
    DWORD type = GetFileType(h);
    return type == FILE_TYPE_DISK || type == FILE_TYPE_PIPE;
}

/* Point the C runtime's descriptor fd, which GHC writes through, and the
 * standard handle std at a file opened anew. */
static void point_at(int fd, DWORD std, const wchar_t *file)
{
    HANDLE h = CreateFileW(file, GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL,
                           OPEN_EXISTING, 0, NULL);
    if (h == INVALID_HANDLE_VALUE)
        return;
    int opened = _open_osfhandle((intptr_t)h, 0);
    if (opened < 0) {
        CloseHandle(h);
        return;
    }
    if (_dup2(opened, fd) == 0)
        SetStdHandle(std, (HANDLE)_get_osfhandle(fd));
    _close(opened);
}

/* In a GUI program, make standard output and error safe to write to: left
 * as they are where they are a file or pipe, else pointed at the console of
 * the terminal this process was started from, if any, else at NUL. Ctrl-C
 * typed in that terminal is meant for whatever it runs now, not trackkat,
 * so it is ignored. This runs before main, so before GHC's runtime starts:
 * the runtime then sets its console handlers up with the console attached,
 * and can take them down again when it exits. */
__attribute__((constructor)) static void attach_console(void)
{
    const IMAGE_DOS_HEADER *dos = (const IMAGE_DOS_HEADER *)GetModuleHandleW(NULL);
    const IMAGE_NT_HEADERS *nt = (const IMAGE_NT_HEADERS *)((const char *)dos + dos->e_lfanew);
    if (nt->OptionalHeader.Subsystem != IMAGE_SUBSYSTEM_WINDOWS_GUI)
        return;
    int out = redirected(STD_OUTPUT_HANDLE), err = redirected(STD_ERROR_HANDLE);
    int attached = 0;
    if (!out || !err) {
        attached = AttachConsole(ATTACH_PARENT_PROCESS);
        if (attached)
            SetConsoleCtrlHandler(NULL, TRUE);
    }
    const wchar_t *elsewhere = attached ? L"CONOUT$" : L"NUL";
    if (!out)
        point_at(1, STD_OUTPUT_HANDLE, elsewhere);
    if (!err)
        point_at(2, STD_ERROR_HANDLE, elsewhere);
    console_output = out ? OUTPUT_REDIRECTED : attached ? OUTPUT_PARENT_CONSOLE : OUTPUT_NONE;
}

/* Where standard output goes, as an OUTPUT_* code. */
int hts_console_output(void)
{
    return console_output;
}
