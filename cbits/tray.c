/* trackkat's system tray icon, on a thread of its own with its own message
 * loop. Haskell (src/TrackKat/Tray.hs) draws the icon and writes its
 * tooltip with hts_tray_show, and polls what is chosen from its menu with
 * hts_tray_take. The window is Haskell's (window/TrackKat/Window.hs). */

#include <windows.h>
#include <shellapi.h>
#include <string.h>
#include <wchar.h>

/* What the user chose, as hts_tray_take reports it. */
#define CHOSE_OPEN 0x1
#define CHOSE_RECENTER 0x2
#define CHOSE_PAUSE 0x4
#define CHOSE_TRIM 0x8
#define CHOSE_QUIT 0x10

#define WM_APP_TRAY (WM_APP + 1)
#define WM_APP_SHOW (WM_APP + 2)   /* from a second trackkat, to open the window */
#define WM_APP_UPDATE (WM_APP + 3) /* hts_tray_show has a new icon */

enum { ID_MENU_OPEN = 300, ID_MENU_RECENTER, ID_MENU_PAUSE, ID_MENU_TRIM, ID_MENU_QUIT };

#define TIP_MAX 128

static HWND tray_hwnd;
static UINT taskbar_created;
static int tray_added;
static NOTIFYICONDATAW nid;
static volatile LONG chosen;
static volatile LONG stopping;
static volatile LONG tray_status; /* 0 starting, 1 running, 2 failed */
static volatile LONG tray_error;

/* The icon to show, from hts_tray_show, under the lock. */
static SRWLOCK lock = SRWLOCK_INIT;
static HICON pending_icon;
static wchar_t pending_tip[TIP_MAX];
static int paused; /* for the menu's tick */

/* An icon from rows of red, green, blue and alpha bytes, top first. */
static HICON icon_from_pixels(const unsigned char *rgba, int size)
{
    BITMAPINFO bi;
    memset(&bi, 0, sizeof bi);
    bi.bmiHeader.biSize = sizeof bi.bmiHeader;
    bi.bmiHeader.biWidth = size;
    bi.bmiHeader.biHeight = -size;
    bi.bmiHeader.biPlanes = 1;
    bi.bmiHeader.biBitCount = 32;
    bi.bmiHeader.biCompression = BI_RGB;
    void *bits;
    HBITMAP colour = CreateDIBSection(NULL, &bi, DIB_RGB_COLORS, &bits, NULL, 0);
    HBITMAP mask = CreateBitmap(size, size, 1, 1, NULL);
    HICON icon = NULL;
    if (colour && mask) {
        DWORD *px = bits;
        for (int i = 0; i < size * size; i++) {
            const unsigned char *p = rgba + 4 * i;
            px[i] = (DWORD)p[3] << 24 | (DWORD)p[0] << 16 | (DWORD)p[1] << 8 | (DWORD)p[2];
        }
        ICONINFO ii;
        memset(&ii, 0, sizeof ii);
        ii.fIcon = TRUE;
        ii.hbmColor = colour;
        ii.hbmMask = mask;
        icon = CreateIconIndirect(&ii);
    }
    if (colour)
        DeleteObject(colour);
    if (mask)
        DeleteObject(mask);
    return icon;
}

/* The tray icon's size in pixels. */
int hts_tray_icon_size(void)
{
    return GetSystemMetrics(SM_CXSMICON);
}

/* Show an icon of hts_tray_icon_size pixels a side, as rows of red, green,
 * blue and alpha bytes, with a tooltip, and whether Pause is ticked in the
 * menu. */
void hts_tray_show(const unsigned char *rgba, int size, const wchar_t *tip, int is_paused)
{
    HICON icon = icon_from_pixels(rgba, size);
    AcquireSRWLockExclusive(&lock);
    if (pending_icon)
        DestroyIcon(pending_icon);
    pending_icon = icon;
    lstrcpynW(pending_tip, tip, TIP_MAX);
    paused = is_paused;
    ReleaseSRWLockExclusive(&lock);
    if (tray_hwnd)
        PostMessageW(tray_hwnd, WM_APP_UPDATE, 0, 0);
}

/* Put the latest icon in the tray, or put it back after Explorer
 * restarted. */
static void update_tray(void)
{
    if (InterlockedCompareExchange(&stopping, 0, 0))
        return;
    AcquireSRWLockExclusive(&lock);
    if (pending_icon) {
        if (nid.hIcon)
            DestroyIcon(nid.hIcon);
        nid.hIcon = pending_icon;
        pending_icon = NULL;
    }
    lstrcpynW(nid.szTip, pending_tip, TIP_MAX);
    ReleaseSRWLockExclusive(&lock);
    if (!nid.hIcon)
        return;
    if (tray_added)
        tray_added = Shell_NotifyIconW(NIM_MODIFY, &nid);
    if (!tray_added)
        tray_added = Shell_NotifyIconW(NIM_ADD, &nid);
}

static void tray_menu(void)
{
    AcquireSRWLockShared(&lock);
    int ticked = paused;
    ReleaseSRWLockShared(&lock);
    HMENU m = CreatePopupMenu();
    AppendMenuW(m, MF_STRING, ID_MENU_OPEN, L"&Open trackkat");
    AppendMenuW(m, MF_STRING, ID_MENU_RECENTER, L"&Recenter");
    AppendMenuW(m, MF_STRING | (ticked ? MF_CHECKED : 0), ID_MENU_PAUSE, L"&Pause");
    AppendMenuW(m, MF_STRING, ID_MENU_TRIM, L"&Trim: make this view forward");
    AppendMenuW(m, MF_SEPARATOR, 0, NULL);
    AppendMenuW(m, MF_STRING, ID_MENU_QUIT, L"&Quit");
    SetMenuDefaultItem(m, ID_MENU_OPEN, FALSE);
    POINT p;
    GetCursorPos(&p);
    /* Without these, the menu doesn't close when clicking elsewhere. */
    SetForegroundWindow(tray_hwnd);
    TrackPopupMenu(m, TPM_RIGHTBUTTON, p.x, p.y, 0, tray_hwnd, NULL);
    PostMessageW(tray_hwnd, WM_NULL, 0, 0);
    DestroyMenu(m);
}

static LRESULT CALLBACK tray_proc(HWND w, UINT msg, WPARAM wp, LPARAM lp)
{
    if (msg == taskbar_created && taskbar_created) {
        /* Explorer restarted, and forgot the icon. */
        tray_added = 0;
        update_tray();
        return 0;
    }
    switch (msg) {
    case WM_APP_UPDATE:
        update_tray();
        return 0;
    case WM_APP_TRAY:
        if (lp == WM_LBUTTONUP || lp == WM_LBUTTONDBLCLK)
            InterlockedOr(&chosen, CHOSE_OPEN);
        else if (lp == WM_RBUTTONUP || lp == WM_CONTEXTMENU)
            tray_menu();
        return 0;
    case WM_APP_SHOW:
        InterlockedOr(&chosen, CHOSE_OPEN);
        return 0;
    case WM_COMMAND:
        switch (LOWORD(wp)) {
        case ID_MENU_OPEN:
            InterlockedOr(&chosen, CHOSE_OPEN);
            break;
        case ID_MENU_RECENTER:
            InterlockedOr(&chosen, CHOSE_RECENTER);
            break;
        case ID_MENU_PAUSE:
            InterlockedOr(&chosen, CHOSE_PAUSE);
            break;
        case ID_MENU_TRIM:
            InterlockedOr(&chosen, CHOSE_TRIM);
            break;
        case ID_MENU_QUIT:
            InterlockedOr(&chosen, CHOSE_QUIT);
            break;
        }
        return 0;
    }
    return DefWindowProcW(w, msg, wp, lp);
}

/* Mark the process aware of each monitor's scaling before any window is
 * made, as GLFW would for the window, so that the tray icon is drawn at
 * the screen's size rather than stretched. */
static void dpi_aware(void)
{
    typedef BOOL(WINAPI * set_context_fn)(HANDLE);
    HMODULE user32 = GetModuleHandleW(L"user32.dll");
    set_context_fn set_context =
        user32 ? (set_context_fn)(void *)GetProcAddress(user32, "SetProcessDpiAwarenessContext") : NULL;
    /* DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2, from Windows 10 1703. */
    if (!set_context || !set_context((HANDLE)-4))
        SetProcessDPIAware();
}

static DWORD WINAPI tray_thread(LPVOID unused)
{
    (void)unused;
    WNDCLASSW wc;
    memset(&wc, 0, sizeof wc);
    wc.hInstance = GetModuleHandleW(NULL);
    wc.lpfnWndProc = tray_proc;
    wc.lpszClassName = L"trackkat";
    /* Hidden, but top-level, so a second trackkat can find it and Explorer's
     * TaskbarCreated reaches it. */
    if (RegisterClassW(&wc))
        tray_hwnd = CreateWindowExW(0, L"trackkat", L"trackkat", WS_OVERLAPPED, 0, 0, 0, 0, NULL, NULL,
                                    wc.hInstance, NULL);
    if (!tray_hwnd) {
        InterlockedExchange(&tray_error, (LONG)GetLastError());
        InterlockedExchange(&tray_status, 2);
        return 0;
    }
    taskbar_created = RegisterWindowMessageW(L"TaskbarCreated");
    nid.cbSize = sizeof nid;
    nid.hWnd = tray_hwnd;
    nid.uID = 1;
    nid.uFlags = NIF_ICON | NIF_TIP | NIF_MESSAGE;
    nid.uCallbackMessage = WM_APP_TRAY;
    update_tray();
    InterlockedExchange(&tray_status, 1);

    MSG msg;
    while (GetMessageW(&msg, NULL, 0, 0) > 0) {
        TranslateMessage(&msg);
        DispatchMessageW(&msg);
    }
    return 0;
}

/* Remove the tray icon, which Windows otherwise leaves behind until the
 * mouse passes over it. */
void hts_tray_stop(void)
{
    InterlockedExchange(&stopping, 1);
    if (tray_hwnd) {
        NOTIFYICONDATAW gone;
        memset(&gone, 0, sizeof gone);
        gone.cbSize = sizeof gone;
        gone.hWnd = tray_hwnd;
        gone.uID = 1;
        Shell_NotifyIconW(NIM_DELETE, &gone);
    }
}

static BOOL WINAPI console_closing(DWORD event)
{
    if (event == CTRL_CLOSE_EVENT || event == CTRL_LOGOFF_EVENT || event == CTRL_SHUTDOWN_EVENT)
        hts_tray_stop();
    return FALSE;
}

/* Start the tray icon on a thread of its own. It shows once hts_tray_show
 * gives it an icon. Returns 0, or a Windows error code. */
int hts_tray_start(void)
{
    dpi_aware();
    SetConsoleCtrlHandler(console_closing, TRUE);
    HANDLE t = CreateThread(NULL, 0, tray_thread, NULL, 0, NULL);
    if (!t)
        return (int)GetLastError();
    CloseHandle(t);
    while (InterlockedCompareExchange(&tray_status, 0, 0) == 0)
        Sleep(1);
    return tray_status == 1 ? 0 : (int)tray_error;
}

/* What was chosen since the last call, as CHOSE_* bits. */
int hts_tray_take(void)
{
    return (int)InterlockedExchange(&chosen, 0);
}

/* Remove the tray icon and end the process at once. */
void hts_tray_quit(void)
{
    hts_tray_stop();
    ExitProcess(0);
}

/* Ask a trackkat already running, if there is one, to open its window.
 * Returns nonzero if one was found. */
int hts_tray_show_running(void)
{
    HWND other = FindWindowW(L"trackkat", NULL);
    if (!other)
        return 0;
    DWORD pid;
    GetWindowThreadProcessId(other, &pid);
    AllowSetForegroundWindow(pid);
    PostMessageW(other, WM_APP_SHOW, 0, 0);
    return 1;
}
