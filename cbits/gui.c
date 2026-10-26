/* trackkat's window and system tray icon, on a thread of their own with its
 * own message loop.
 *
 * Haskell pushes state in with the hts_gui_* setters and polls what the user
 * asked for with hts_gui_take_actions, as with the hotkeys. The GUI thread
 * holds the lock only to copy state in or out, and the setter called on every
 * frame gives up rather than wait for it, so drawing never delays tracking. */

#include <windows.h>
#include <commctrl.h>
#include <shellapi.h>
#include <math.h>
#include <string.h>
#include <wchar.h>

unsigned hts_modifiers_held(void); /* cbits/windows.c */

#define PI 3.14159265358979323846

/* What the user asked for, as hts_gui_take_actions reports it. */
#define ACT_RECENTER 0x1
#define ACT_PAUSE 0x2
#define ACT_CALIBRATE 0x4
#define ACT_QUIT 0x8
#define ACT_SAVE 0x10
#define ACT_KEY_RECENTER 0x20 /* a key was pressed to add to the bindings */
#define ACT_KEY_PAUSE 0x40

/* The camera's state, as hts_gui_camera takes it. */
enum { CAMERA_SEARCHING, CAMERA_TRACKING, CAMERA_DOWN };

#define TEXT_MAX 512
#define FIELD_MAX 256

/* The settings the window edits, by their names in trackkat.conf. */
enum { F_RECENTER, F_PAUSE, F_SMOOTHING, F_STEADINESS, F_RESPONSIVENESS, F_YAW, F_PITCH, F_ROLL, F_X, F_Y, F_Z, F_COUNT };
static const wchar_t *const field_names[F_COUNT] = {L"recenter", L"pause", L"smoothing", L"steadiness",
                                                    L"responsiveness", L"yaw", L"pitch", L"roll", L"x", L"y", L"z"};

typedef struct State {
    int camera;
    wchar_t camera_text[TEXT_MAX]; /* why the camera is down */
    int blobs;                     /* -1 while the clip is in sight, else how many reflectors are */
    float head[6];                 /* yaw, pitch, roll in degrees, x, y, z in mm */
    float view[6];                 /* the same, as games see it */
    int paused;
    int games;
    wchar_t stats[TEXT_MAX];
    wchar_t calibration[TEXT_MAX];
    wchar_t settings_message[TEXT_MAX];
} State;

/* ---- Shared with Haskell, under the lock --------------------------------- */

static SRWLOCK lock = SRWLOCK_INIT;
static State shared = {CAMERA_SEARCHING, L"", 0, {0}, {0}, 0, 0, L"", L"",
                       L"Separate several keys or buttons with commas. Curves are input:output points, in degrees or mm."};
static wchar_t pushed[F_COUNT][FIELD_MAX]; /* settings to show */
static unsigned pushed_fields;             /* bit per field pushed since the GUI last looked */
static wchar_t saved[F_COUNT][FIELD_MAX];  /* settings as the user asked to save them */

/* The log, as a window of the last LOG_MAX characters of everything logged.
 * log_total counts every character ever logged, so the GUI can tell what it
 * has shown even after the start of the window is dropped. */
#define LOG_MAX (256 * 1024)
static wchar_t log_text[LOG_MAX];
static size_t log_len;
static unsigned long long log_total;

static volatile LONG actions;
static volatile LONG chosen_keys[2]; /* modifiers << 16 | virtual key, per binding list */
static volatile LONG capturing = -1;  /* the binding list waiting for a key or button, or -1 */
static wchar_t added[2][FIELD_MAX];   /* bindings to add to the lists */
static unsigned added_lists;          /* bit per list with a binding to add */
static volatile LONG stopping;

static void copy_text(wchar_t *dst, size_t size, const wchar_t *src)
{
    size_t n = wcslen(src);
    if (n >= size)
        n = size - 1;
    memcpy(dst, src, n * sizeof *dst);
    dst[n] = 0;
}

void hts_gui_log(const wchar_t *line)
{
    size_t n = wcslen(line);
    if (n > LOG_MAX / 4)
        n = LOG_MAX / 4;
    AcquireSRWLockExclusive(&lock);
    if (log_len + n + 2 > LOG_MAX) {
        /* Drop the older half, at a line break. */
        size_t drop = log_len / 2;
        while (drop < log_len && log_text[drop - 1] != L'\n')
            drop++;
        memmove(log_text, log_text + drop, (log_len - drop) * sizeof *log_text);
        log_len -= drop;
    }
    memcpy(log_text + log_len, line, n * sizeof *line);
    log_text[log_len + n] = L'\r';
    log_text[log_len + n + 1] = L'\n';
    log_len += n + 2;
    log_total += n + 2;
    ReleaseSRWLockExclusive(&lock);
}

/* Called on every frame, so it skips the update rather than wait for the
 * GUI to finish copying. The next frame's comes soon enough. */
void hts_gui_frame(int blobs, float yaw, float pitch, float roll, float x, float y, float z, float vyaw,
                   float vpitch, float vroll, float vx, float vy, float vz)
{
    if (!TryAcquireSRWLockExclusive(&lock))
        return;
    shared.blobs = blobs;
    float h[6] = {yaw, pitch, roll, x, y, z}, v[6] = {vyaw, vpitch, vroll, vx, vy, vz};
    memcpy(shared.head, h, sizeof h);
    memcpy(shared.view, v, sizeof v);
    ReleaseSRWLockExclusive(&lock);
}

void hts_gui_camera(int camera, const wchar_t *text)
{
    AcquireSRWLockExclusive(&lock);
    shared.camera = camera;
    copy_text(shared.camera_text, TEXT_MAX, text);
    ReleaseSRWLockExclusive(&lock);
}

void hts_gui_paused(int paused)
{
    AcquireSRWLockExclusive(&lock);
    shared.paused = paused;
    ReleaseSRWLockExclusive(&lock);
}

void hts_gui_games(int games)
{
    AcquireSRWLockExclusive(&lock);
    shared.games = games;
    ReleaseSRWLockExclusive(&lock);
}

static void set_text(wchar_t *place, const wchar_t *text)
{
    AcquireSRWLockExclusive(&lock);
    copy_text(place, TEXT_MAX, text);
    ReleaseSRWLockExclusive(&lock);
}

void hts_gui_stats(const wchar_t *text) { set_text(shared.stats, text); }
void hts_gui_calibration(const wchar_t *text) { set_text(shared.calibration, text); }
void hts_gui_settings_message(const wchar_t *text) { set_text(shared.settings_message, text); }

static int field_index(const wchar_t *name)
{
    for (int i = 0; i < F_COUNT; i++)
        if (wcscmp(name, field_names[i]) == 0)
            return i;
    return -1;
}

/* Show a setting's value in the window. Unknown names are ignored. */
void hts_gui_set_setting(const wchar_t *name, const wchar_t *value)
{
    int i = field_index(name);
    if (i < 0)
        return;
    AcquireSRWLockExclusive(&lock);
    copy_text(pushed[i], FIELD_MAX, value);
    pushed_fields |= 1u << i;
    ReleaseSRWLockExclusive(&lock);
}

/* A setting's value as the user last asked to save it, or "" for an unknown
 * name. */
void hts_gui_saved_setting(const wchar_t *name, wchar_t *buf, int size)
{
    int i = field_index(name);
    AcquireSRWLockExclusive(&lock);
    copy_text(buf, (size_t)size, i < 0 ? L"" : saved[i]);
    ReleaseSRWLockExclusive(&lock);
}

/* What the user asked for since the last call, as ACT_* bits. */
int hts_gui_take_actions(void)
{
    return (int)InterlockedExchange(&actions, 0);
}

/* The last key pressed to add to a binding list (0 recenter, 1 pause), as
 * modifiers << 16 | virtual key. */
int hts_gui_chosen_key(int which)
{
    return (int)InterlockedCompareExchange(&chosen_keys[which ? 1 : 0], 0, 0);
}

/* Which binding list waits for a key or joystick button to add: 0 recenter,
 * 1 pause, or -1 none. */
int hts_gui_capturing(void)
{
    return (int)InterlockedCompareExchange(&capturing, 0, 0);
}

/* Add a binding to a list in the window (0 recenter, 1 pause), as written in
 * trackkat.conf, and stop waiting for one. */
void hts_gui_add_binding(int which, const wchar_t *binding)
{
    which = which ? 1 : 0;
    AcquireSRWLockExclusive(&lock);
    copy_text(added[which], FIELD_MAX, binding);
    added_lists |= 1u << which;
    ReleaseSRWLockExclusive(&lock);
}

/* ---- The GUI thread's own state ------------------------------------------ */

#define WM_APP_TRAY (WM_APP + 1)
#define WM_APP_SHOW (WM_APP + 2)
#define TIMER_ID 1
#define TIMER_MS 33

enum {
    ID_RECENTER = 100,
    ID_PAUSE,
    ID_CALIBRATE,
    ID_COPY_LOG,
    ID_ADD_RECENTER,
    ID_ADD_PAUSE,
    ID_SMOOTHING,
    ID_FIELD = 200, /* + field index */
    ID_MENU_OPEN = 300,
    ID_MENU_RECENTER,
    ID_MENU_PAUSE,
    ID_MENU_QUIT,
};

static HINSTANCE inst;
static HWND tray_hwnd, main_hwnd, live_hwnd;
static HWND recenter_button, pause_button, calibration_text, settings_text, log_box;
static HWND log_group, copy_button, log_hint;
static HWND field_ctl[F_COUNT];
static HWND slider_value[2]; /* steadiness and responsiveness, as numbers */
static wchar_t field_text[F_COUNT][FIELD_MAX]; /* values of the checkbox and sliders */
static HWND add_button[2];
static WNDPROC button_proc;
static int swallow_key_up; /* the key that was captured is still to come up */
static State seen; /* the last snapshot */
static int dpi = 96;
static HFONT ui_font, mono_font, small_font, bold_font;
static HBRUSH log_brush;
static UINT taskbar_created;
static int tray_added;
static NOTIFYICONDATAW nid;
static int icon_shown = -1;
static wchar_t tip_shown[128];
static unsigned long long log_shown; /* log characters put in the log box, counting all ever logged */

static volatile LONG gui_status; /* 0 starting, 1 running, 2 failed */
static volatile LONG gui_error;
static int show_at_start;

static int S(int x) { return MulDiv(x, dpi, 96); }

/* ---- Icons ---------------------------------------------------------------- */

/* Icons are a disc coloured by state: green tracking, amber with the clip out
 * of sight, grey looking for the camera, red without one. A white dot means a
 * game is connected (like the camera's second light); white bars mean paused. */
enum { LOOK_TRACKING, LOOK_CLIP_LOST, LOOK_SEARCHING, LOOK_DOWN };
static const COLORREF look_colour[4] = {RGB(60, 180, 100), RGB(235, 160, 40), RGB(140, 145, 155), RGB(215, 70, 60)};

static double coverage(double inside) { return inside < 0 ? 0 : inside > 1 ? 1 : inside; }

static HICON make_icon(int size, int look, int mark)
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
    if (!colour || !mask)
        return NULL;
    DWORD *px = bits;
    double c = size / 2.0, r = size / 2.0 - 0.5, edge = size / 16.0;
    COLORREF fill = look_colour[look];
    for (int j = 0; j < size; j++)
        for (int i = 0; i < size; i++) {
            double dx = i + 0.5 - c, dy = j + 0.5 - c, d = sqrt(dx * dx + dy * dy);
            double a = coverage(r - d + 0.5);
            /* A darker rim, so the disc shows on light and dark taskbars. */
            double rim = coverage(d - (r - edge) + 0.5) * 0.45;
            double cr = GetRValue(fill) * (1 - rim), cg = GetGValue(fill) * (1 - rim), cb = GetBValue(fill) * (1 - rim);
            double w = 0;
            if (mark == 1)
                w = coverage(size * 0.2 - d + 0.5);
            else if (mark == 2) {
                double bx = fabs(fabs(dx) - size * 0.14), by = fabs(dy);
                w = coverage(size * 0.07 - bx + 0.5) * coverage(size * 0.24 - by + 0.5);
            }
            cr += (255 - cr) * w;
            cg += (255 - cg) * w;
            cb += (255 - cb) * w;
            px[j * size + i] = (DWORD)(a * 255) << 24 | (DWORD)cr << 16 | (DWORD)cg << 8 | (DWORD)cb;
        }
    ICONINFO ii;
    memset(&ii, 0, sizeof ii);
    ii.fIcon = TRUE;
    ii.hbmColor = colour;
    ii.hbmMask = mask;
    HICON icon = CreateIconIndirect(&ii);
    DeleteObject(colour);
    DeleteObject(mask);
    return icon;
}

static int current_look(const State *st)
{
    if (st->camera == CAMERA_TRACKING)
        return st->blobs < 0 ? LOOK_TRACKING : LOOK_CLIP_LOST;
    return st->camera == CAMERA_SEARCHING ? LOOK_SEARCHING : LOOK_DOWN;
}

static int current_mark(const State *st) { return st->paused ? 2 : st->games > 0 ? 1 : 0; }

/* ---- Tray ----------------------------------------------------------------- */

static void describe(const State *st, wchar_t *buf, size_t size)
{
    const wchar_t *what = st->camera == CAMERA_SEARCHING ? L"Looking for the camera"
                          : st->camera == CAMERA_DOWN    ? L"No camera"
                          : st->blobs < 0                ? L"Tracking"
                                                         : L"Clip out of sight";
    _snwprintf(buf, size, L"trackkat\n%ls%ls%ls", what, st->games > 0 ? L"\nGame connected" : L"",
               st->paused ? L"\nPaused" : L"");
    buf[size - 1] = 0;
}

static void update_tray(int force)
{
    if (InterlockedCompareExchange(&stopping, 0, 0))
        return;
    int look = current_look(&seen), mark = current_mark(&seen);
    wchar_t tip[128];
    describe(&seen, tip, 128);
    int key = look * 3 + mark;
    if (!force && tray_added && key == icon_shown && wcscmp(tip, tip_shown) == 0)
        return;
    HICON old = nid.hIcon;
    nid.hIcon = make_icon(GetSystemMetrics(SM_CXSMICON), look, mark);
    copy_text(nid.szTip, 128, tip);
    if (tray_added)
        tray_added = Shell_NotifyIconW(NIM_MODIFY, &nid);
    if (!tray_added)
        tray_added = Shell_NotifyIconW(NIM_ADD, &nid);
    if (old)
        DestroyIcon(old);
    if (key != icon_shown && main_hwnd) {
        HICON big = make_icon(GetSystemMetrics(SM_CXICON), look, mark);
        HICON small = make_icon(GetSystemMetrics(SM_CXSMICON), look, mark);
        HICON old_big = (HICON)SendMessageW(main_hwnd, WM_SETICON, ICON_BIG, (LPARAM)big);
        HICON old_small = (HICON)SendMessageW(main_hwnd, WM_SETICON, ICON_SMALL, (LPARAM)small);
        if (old_big)
            DestroyIcon(old_big);
        if (old_small)
            DestroyIcon(old_small);
    }
    icon_shown = key;
    copy_text(tip_shown, 128, tip);
}

static void show_window(void)
{
    if (IsIconic(main_hwnd))
        ShowWindow(main_hwnd, SW_RESTORE);
    else
        ShowWindow(main_hwnd, SW_SHOW);
    SetForegroundWindow(main_hwnd);
}

static void tray_menu(void)
{
    HMENU m = CreatePopupMenu();
    AppendMenuW(m, MF_STRING, ID_MENU_OPEN, L"&Open trackkat");
    AppendMenuW(m, MF_STRING, ID_MENU_RECENTER, L"&Recenter");
    AppendMenuW(m, MF_STRING | (seen.paused ? MF_CHECKED : 0), ID_MENU_PAUSE, L"&Pause");
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

/* ---- The live view -------------------------------------------------------- */

#define C_BACK RGB(30, 34, 40)
#define C_BOX RGB(38, 43, 51)
#define C_GRID RGB(56, 62, 72)
#define C_TEXT RGB(222, 226, 232)
#define C_DIM RGB(135, 142, 154)
#define C_ACCENT RGB(90, 170, 255)
#define C_HEAD_TICK RGB(255, 200, 80)
#define C_GREEN RGB(80, 200, 120)
#define C_AMBER RGB(240, 170, 60)
#define C_RED RGB(230, 85, 75)
#define C_GREY RGB(120, 126, 138)

/* Rotate a point on the head, in head axes (x the user's right, y up, z out
 * of the face), by the view's roll, then pitch, then yaw, as seen in a
 * mirror: turning right turns the drawing to the right. */
static void turn(const float view[6], double p[3])
{
    double a = view[0] * PI / 180, b = view[1] * PI / 180, c = view[2] * PI / 180;
    double x = p[0], y = p[1], z = p[2], t;
    t = x * cos(c) - y * sin(c); /* roll left is positive */
    y = x * sin(c) + y * cos(c);
    x = t;
    t = y * cos(b) - z * sin(b); /* pitch down is positive */
    z = y * sin(b) + z * cos(b);
    y = t;
    t = x * cos(a) + z * sin(a); /* yaw right is positive */
    z = -x * sin(a) + z * cos(a);
    x = t;
    p[0] = x;
    p[1] = y;
    p[2] = z;
}

typedef struct {
    double cx, cy, r;
} Head;

/* Where a point on the head lands on screen, and whether it faces us. */
static int project(const Head *h, const float view[6], double x, double y, double z, POINT *out)
{
    double p[3] = {x, y, z};
    turn(view, p);
    out->x = (LONG)lround(h->cx + p[0] * h->r);
    out->y = (LONG)lround(h->cy - p[1] * h->r);
    return p[2] > 0;
}

static void pen_line(HDC dc, COLORREF c, int width, POINT a, POINT b)
{
    HPEN pen = CreatePen(PS_SOLID, width, c);
    HGDIOBJ old = SelectObject(dc, pen);
    MoveToEx(dc, a.x, a.y, NULL);
    LineTo(dc, b.x, b.y);
    SelectObject(dc, old);
    DeleteObject(pen);
}

static void disc(HDC dc, COLORREF fill, COLORREF line, int width, int cx, int cy, int r)
{
    HBRUSH brush = CreateSolidBrush(fill);
    HPEN pen = CreatePen(PS_SOLID, width, line);
    HGDIOBJ ob = SelectObject(dc, brush), op = SelectObject(dc, pen);
    Ellipse(dc, cx - r, cy - r, cx + r + 1, cy + r + 1);
    SelectObject(dc, ob);
    SelectObject(dc, op);
    DeleteObject(brush);
    DeleteObject(pen);
}

static void fill_rect(HDC dc, int l, int t, int r, int b, COLORREF c)
{
    RECT rc = {l, t, r, b};
    HBRUSH brush = CreateSolidBrush(c);
    FillRect(dc, &rc, brush);
    DeleteObject(brush);
}

static void text_at(HDC dc, HFONT font, COLORREF c, int x, int y, int w, int h, UINT align, const wchar_t *s)
{
    RECT rc = {x, y, x + w, y + h};
    SelectObject(dc, font);
    SetTextColor(dc, c);
    DrawTextW(dc, s, -1, &rc, align | DT_SINGLELINE | DT_VCENTER | DT_NOPREFIX | DT_END_ELLIPSIS);
}

/* The head as games see it: a face that turns, tilts and moves with the
 * view, a line out of the nose showing where it looks. */
static void draw_head(HDC dc, int left, int top, int size)
{
    const float *v = seen.view;
    int live = seen.camera == CAMERA_TRACKING && seen.blobs < 0;
    fill_rect(dc, left, top, left + size, top + size, C_BOX);
    HPEN grid = CreatePen(PS_DOT, 1, C_GRID);
    HGDIOBJ old = SelectObject(dc, grid);
    MoveToEx(dc, left + size / 2, top, NULL);
    LineTo(dc, left + size / 2, top + size);
    MoveToEx(dc, left, top + size / 2, NULL);
    LineTo(dc, left + size, top + size / 2);
    SelectObject(dc, old);
    DeleteObject(grid);

    /* 300 mm from the centre to the edge; nearer is bigger. */
    double k = size / 2.0 / 300.0;
    double z = v[5] < -300 ? -300 : v[5];
    Head h = {left + size / 2.0 - v[3] * k, top + size / 2.0 + v[4] * k, size * 0.2 * 600.0 / (600.0 + z)};
    HRGN clip = CreateRectRgn(left, top, left + size, top + size);
    SelectClipRgn(dc, clip);

    COLORREF skin = live ? RGB(78, 88, 104) : RGB(58, 62, 70);
    COLORREF line = live ? RGB(205, 212, 224) : C_DIM;
    COLORREF feature = live ? RGB(240, 244, 250) : C_DIM;
    int w = S(2);
    POINT centre = {(LONG)lround(h.cx), (LONG)lround(h.cy)}, p, q;

    /* What is behind the head is drawn before it, what faces us after. */
    for (int pass = 0; pass < 2; pass++) {
        if (pass == 1)
            disc(dc, skin, line, w, centre.x, centre.y, (int)lround(h.r));
        for (int side = -1; side <= 1; side += 2)
            if (project(&h, v, side * 0.98, 0, 0, &p) == pass)
                disc(dc, skin, line, w, p.x, p.y, (int)lround(h.r * 0.2));
        int front = project(&h, v, 0, -0.05, 1, &p);
        if (front == pass) {
            project(&h, v, 0, -0.05, 2.2, &q);
            pen_line(dc, live ? C_ACCENT : C_DIM, w, p, q);
        }
    }
    for (int side = -1; side <= 1; side += 2)
        if (project(&h, v, side * 0.36, 0.22, 0.9, &p))
            disc(dc, feature, feature, 1, p.x, p.y, (int)lround(h.r * 0.09));
    if (project(&h, v, 0, -0.05, 1, &p)) {
        project(&h, v, 0, -0.12, 1.28, &q);
        pen_line(dc, feature, w, p, q);
    }
    POINT prev;
    int prev_front = 0;
    for (int i = 0; i <= 8; i++) {
        double x = -0.32 + 0.64 * i / 8, y = -0.48 + 0.08 * sin(i * PI / 8) * -1;
        double zz = sqrt(1 - x * x - y * y);
        int front = project(&h, v, x, y, zz, &p);
        if (i > 0 && front && prev_front)
            pen_line(dc, feature, w, prev, p);
        prev = p;
        prev_front = front;
    }
    SelectClipRgn(dc, NULL);
    DeleteObject(clip);

    if (!live) {
        const wchar_t *why = seen.camera == CAMERA_TRACKING ? L"Clip out of sight" : L"No tracking";
        text_at(dc, bold_font, C_AMBER, left, top + size - S(28), size, S(24), DT_CENTER, why);
    } else if (seen.paused)
        text_at(dc, bold_font, C_AMBER, left, top + size - S(28), size, S(24), DT_CENTER, L"PAUSED");
}

static const wchar_t *const axis_names[6] = {L"Yaw", L"Pitch", L"Roll", L"X", L"Y", L"Z"};
/* What each axis's sides mean, as trackkat measures them. */
static const wchar_t *const axis_minus[6] = {L"left", L"up", L"right", L"right", L"up", L"fwd"};
static const wchar_t *const axis_plus[6] = {L"right", L"down", L"left", L"left", L"down", L"back"};
static const float axis_range[6] = {180, 90, 90, 300, 300, 300};

static void format_axis(wchar_t *buf, size_t size, int axis, float value)
{
    if (axis < 3)
        _snwprintf(buf, size, L"%+.1f°", value);
    else
        _snwprintf(buf, size, L"%+.0f mm", value);
    buf[size - 1] = 0;
}

static void status_line(HDC dc, int x, int y, COLORREF dot, const wchar_t *label, const wchar_t *value)
{
    disc(dc, dot, dot, 1, x + S(5), y + S(9), S(4));
    text_at(dc, small_font, C_DIM, x + S(16), y, S(64), S(18), DT_LEFT, label);
    text_at(dc, ui_font, C_TEXT, x + S(80), y, S(360), S(18), DT_LEFT, value);
}

static void paint_live(HDC dc, int width, int height)
{
    fill_rect(dc, 0, 0, width, height, C_BACK);
    SetBkMode(dc, TRANSPARENT);
    int size = height - S(16);
    draw_head(dc, S(8), S(8), size);

    int x0 = S(8) + size + S(16);
    int tracking = seen.camera == CAMERA_TRACKING && seen.blobs < 0;
    text_at(dc, small_font, C_DIM, x0 + S(50), S(6), S(80), S(18), DT_RIGHT, L"head");
    text_at(dc, small_font, C_DIM, x0 + S(130), S(6), S(80), S(18), DT_RIGHT, L"games see");
    int bar_l = x0 + S(226), bar_r = width - S(10);
    for (int i = 0; i < 6; i++) {
        int y = S(26) + i * S(20);
        wchar_t buf[32];
        text_at(dc, ui_font, C_TEXT, x0, y, S(50), S(20), DT_LEFT, axis_names[i]);
        format_axis(buf, 32, i, seen.head[i]);
        text_at(dc, mono_font, tracking ? C_TEXT : C_DIM, x0 + S(50), y, S(80), S(20), DT_RIGHT, buf);
        format_axis(buf, 32, i, seen.view[i]);
        text_at(dc, mono_font, tracking || seen.paused ? C_ACCENT : C_DIM, x0 + S(130), y, S(80), S(20), DT_RIGHT,
                buf);

        int top = y + S(3), bottom = y + S(16), mid = (bar_l + bar_r) / 2, half = (bar_r - bar_l) / 2;
        fill_rect(dc, bar_l, top, bar_r, bottom, C_BOX);
        double f = seen.view[i] / axis_range[i];
        f = f < -1 ? -1 : f > 1 ? 1 : f;
        int end = mid + (int)lround(f * half);
        fill_rect(dc, end < mid ? end : mid, top, end < mid ? mid : end + 1, bottom, C_ACCENT);
        double g = seen.head[i] / axis_range[i];
        g = g < -1 ? -1 : g > 1 ? 1 : g;
        int tick = mid + (int)lround(g * half);
        fill_rect(dc, tick - S(1), top - S(2), tick + S(1) + 1, bottom + S(2), C_HEAD_TICK);
        fill_rect(dc, mid, top - S(2), mid + 1, bottom + S(2), C_GRID);
        text_at(dc, small_font, C_DIM, bar_l + S(4), top, S(50), bottom - top, DT_LEFT, axis_minus[i]);
        text_at(dc, small_font, C_DIM, bar_r - S(54), top, S(50), bottom - top, DT_RIGHT, axis_plus[i]);
    }

    int y = S(26) + 6 * S(20) + S(6);
    wchar_t buf[TEXT_MAX + 64];
    switch (seen.camera) {
    case CAMERA_TRACKING:
        status_line(dc, x0, y, C_GREEN, L"Camera", L"Connected");
        break;
    case CAMERA_SEARCHING:
        status_line(dc, x0, y, C_GREY, L"Camera", L"Looking for it…");
        break;
    default:
        status_line(dc, x0, y, C_RED, L"Camera", seen.camera_text[0] ? seen.camera_text : L"Not found");
    }
    y += S(18);
    if (seen.camera != CAMERA_TRACKING)
        status_line(dc, x0, y, C_GREY, L"Clip", L"—");
    else if (seen.blobs < 0)
        status_line(dc, x0, y, C_GREEN, L"Clip", L"In sight");
    else {
        _snwprintf(buf, 64, L"Out of sight (%d of 3 reflectors)", seen.blobs);
        buf[63] = 0;
        status_line(dc, x0, y, C_AMBER, L"Clip", buf);
    }
    y += S(18);
    if (seen.games > 0) {
        _snwprintf(buf, 64, seen.games == 1 ? L"1 connected" : L"%d connected", seen.games);
        buf[63] = 0;
    }
    status_line(dc, x0, y, seen.games > 0 ? C_ACCENT : C_GREY, L"Games", seen.games > 0 ? buf : L"None");
    y += S(18);
    status_line(dc, x0, y, seen.paused ? C_AMBER : C_GREEN, L"Output",
                seen.paused ? L"Paused: games see your head held still" : L"Live");
    y += S(18);
    if (seen.camera == CAMERA_TRACKING && seen.stats[0])
        text_at(dc, small_font, C_DIM, x0, y, width - x0 - S(10), S(18), DT_LEFT, seen.stats);
}

static LRESULT CALLBACK live_proc(HWND w, UINT msg, WPARAM wp, LPARAM lp)
{
    switch (msg) {
    case WM_ERASEBKGND:
        return 1;
    case WM_PAINT: {
        PAINTSTRUCT ps;
        HDC dc = BeginPaint(w, &ps);
        RECT rc;
        GetClientRect(w, &rc);
        /* Drawn off screen first, so it doesn't flicker. */
        HDC mem = CreateCompatibleDC(dc);
        HBITMAP bmp = CreateCompatibleBitmap(dc, rc.right, rc.bottom);
        HGDIOBJ old = SelectObject(mem, bmp);
        paint_live(mem, rc.right, rc.bottom);
        BitBlt(dc, 0, 0, rc.right, rc.bottom, mem, 0, 0, SRCCOPY);
        SelectObject(mem, old);
        DeleteObject(bmp);
        DeleteDC(mem);
        EndPaint(w, &ps);
        return 0;
    }
    }
    return DefWindowProcW(w, msg, wp, lp);
}

/* ---- Settings ------------------------------------------------------------- */

static int is_modifier(WPARAM vk)
{
    return vk == VK_SHIFT || vk == VK_CONTROL || vk == VK_MENU || vk == VK_LWIN || vk == VK_RWIN ||
           vk == VK_LSHIFT || vk == VK_RSHIFT || vk == VK_LCONTROL || vk == VK_RCONTROL || vk == VK_LMENU ||
           vk == VK_RMENU;
}

static void end_capture(void)
{
    LONG which = InterlockedExchange(&capturing, -1);
    if (which >= 0)
        SetWindowTextW(add_button[which], L"Add key or button…");
}

static void start_capture(int which)
{
    end_capture();
    InterlockedExchange(&capturing, which);
    SetWindowTextW(add_button[which], L"Press it now…");
    SetFocus(add_button[which]);
}

/* An Add button, once clicked, takes the next key pressed (and Haskell the
 * next joystick button) and adds it to its list. Clicking elsewhere stops
 * it waiting. */
static LRESULT CALLBACK add_button_proc(HWND w, UINT msg, WPARAM wp, LPARAM lp)
{
    int which = GetDlgCtrlID(w) == ID_ADD_PAUSE;
    int waiting = InterlockedCompareExchange(&capturing, 0, 0) == which;
    switch (msg) {
    case WM_GETDLGCODE:
        if (waiting)
            return DLGC_WANTALLKEYS;
        break;
    case WM_KEYDOWN:
    case WM_SYSKEYDOWN:
        if (!waiting)
            break;
        if (!is_modifier(wp)) {
            InterlockedExchange(&chosen_keys[which], (LONG)(hts_modifiers_held() << 16 | (unsigned)wp));
            InterlockedOr(&actions, which ? ACT_KEY_PAUSE : ACT_KEY_RECENTER);
            swallow_key_up = 1;
            end_capture();
        }
        return 0;
    case WM_CHAR:
    case WM_SYSCHAR:
        if (waiting || swallow_key_up)
            return 0;
        break;
    case WM_KEYUP:
    case WM_SYSKEYUP:
        if (waiting || swallow_key_up) {
            if (!is_modifier(wp))
                swallow_key_up = 0;
            return 0;
        }
        break;
    case WM_KILLFOCUS:
        if (waiting)
            end_capture();
        break;
    }
    return CallWindowProcW(button_proc, w, msg, wp, lp);
}

/* Add a binding to a list's text, unless it is there already. */
static void add_binding(int field, const wchar_t *binding)
{
    wchar_t text[FIELD_MAX];
    GetWindowTextW(field_ctl[field], text, FIELD_MAX);
    const wchar_t *p = text;
    size_t blen = wcslen(binding);
    int empty = 1;
    while (*p) {
        while (*p == L' ' || *p == L',')
            p++;
        const wchar_t *end = p;
        while (*end && *end != L',')
            end++;
        const wchar_t *last = end;
        while (last > p && last[-1] == L' ')
            last--;
        if (last > p)
            empty = 0;
        if ((size_t)(last - p) == blen && _wcsnicmp(p, binding, blen) == 0)
            return;
        p = end;
    }
    wchar_t joined[FIELD_MAX];
    if (empty)
        copy_text(joined, FIELD_MAX, binding);
    else {
        _snwprintf(joined, FIELD_MAX, L"%ls, %ls", text, binding);
        joined[FIELD_MAX - 1] = 0;
    }
    SetWindowTextW(field_ctl[field], joined);
}

/* A button label naming a binding list's first binding. */
static void binding_label(wchar_t *label, size_t size, const wchar_t *action, const wchar_t *bindings)
{
    wchar_t first[FIELD_MAX];
    copy_text(first, FIELD_MAX, bindings);
    wchar_t *comma = wcschr(first, L',');
    if (comma)
        *comma = 0;
    if (first[0])
        _snwprintf(label, size, comma ? L"%ls (%ls, …)" : L"%ls (%ls)", action, first);
    else
        _snwprintf(label, size, L"%ls", action);
    label[size - 1] = 0;
}

static int slider_field(int i) { return i == F_STEADINESS || i == F_RESPONSIVENESS; }

/* Show a setting pushed from Haskell. */
static void show_field(int i, const wchar_t *value)
{
    copy_text(field_text[i], FIELD_MAX, value);
    if (i == F_SMOOTHING) {
        int on = wcscmp(value, L"off") != 0;
        SendMessageW(field_ctl[i], BM_SETCHECK, on ? BST_CHECKED : BST_UNCHECKED, 0);
        EnableWindow(field_ctl[F_STEADINESS], on);
        EnableWindow(field_ctl[F_RESPONSIVENESS], on);
    } else if (slider_field(i)) {
        SendMessageW(field_ctl[i], TBM_SETPOS, TRUE, (LPARAM)lround(_wtof(value)));
        SetWindowTextW(slider_value[i == F_RESPONSIVENESS], value);
    } else
        SetWindowTextW(field_ctl[i], value);
    if (i == F_RECENTER || i == F_PAUSE) {
        wchar_t label[FIELD_MAX + 32];
        binding_label(label, FIELD_MAX + 32, i == F_RECENTER ? L"Recenter" : seen.paused ? L"Resume" : L"Pause", value);
        SetWindowTextW(i == F_RECENTER ? recenter_button : pause_button, label);
    }
}

static void save_settings(void)
{
    wchar_t values[F_COUNT][FIELD_MAX];
    for (int i = 0; i < F_COUNT; i++)
        if (i == F_SMOOTHING || slider_field(i))
            copy_text(values[i], FIELD_MAX, field_text[i]);
        else
            GetWindowTextW(field_ctl[i], values[i], FIELD_MAX);
    AcquireSRWLockExclusive(&lock);
    memcpy(saved, values, sizeof saved);
    ReleaseSRWLockExclusive(&lock);
    InterlockedOr(&actions, ACT_SAVE);
}

/* ---- The log box ---------------------------------------------------------- */

static void append_log(const wchar_t *text)
{
    SendMessageW(log_box, WM_SETREDRAW, FALSE, 0);
    DWORD start, end;
    SendMessageW(log_box, EM_GETSEL, (WPARAM)&start, (LPARAM)&end);
    SCROLLINFO si;
    si.cbSize = sizeof si;
    si.fMask = SIF_ALL;
    GetScrollInfo(log_box, SB_VERT, &si);
    int following = start == end && (si.nPage == 0 || si.nPos + (int)si.nPage >= si.nMax);
    int first = (int)SendMessageW(log_box, EM_GETFIRSTVISIBLELINE, 0, 0);

    /* Keep the box to about the log's length, dropping whole lines. */
    int len = GetWindowTextLengthW(log_box);
    if (len > LOG_MAX) {
        int cut_line = (int)SendMessageW(log_box, EM_LINEFROMCHAR, (WPARAM)(len - LOG_MAX * 3 / 4), 0) + 1;
        int cut = (int)SendMessageW(log_box, EM_LINEINDEX, (WPARAM)cut_line, 0);
        if (cut > 0) {
            SendMessageW(log_box, EM_SETSEL, 0, cut);
            SendMessageW(log_box, EM_REPLACESEL, FALSE, (LPARAM)L"");
            start = start > (DWORD)cut ? start - (DWORD)cut : 0;
            end = end > (DWORD)cut ? end - (DWORD)cut : 0;
            first = first > cut_line ? first - cut_line : 0;
            len -= cut;
        }
    }
    SendMessageW(log_box, EM_SETSEL, (WPARAM)len, (LPARAM)len);
    SendMessageW(log_box, EM_REPLACESEL, FALSE, (LPARAM)text);
    if (following) {
        int end_pos = GetWindowTextLengthW(log_box);
        SendMessageW(log_box, EM_SETSEL, (WPARAM)end_pos, (LPARAM)end_pos);
        SendMessageW(log_box, EM_SCROLLCARET, 0, 0);
    } else {
        /* Leave the user's selection and scroll position alone. */
        SendMessageW(log_box, EM_SETSEL, start, end);
        int now = (int)SendMessageW(log_box, EM_GETFIRSTVISIBLELINE, 0, 0);
        SendMessageW(log_box, EM_LINESCROLL, 0, first - now);
    }
    SendMessageW(log_box, WM_SETREDRAW, TRUE, 0);
    InvalidateRect(log_box, NULL, TRUE);
}

/* Move newly logged text into the log box, a bounded amount at a time so
 * the lock is held only briefly. */
static void drain_log(void)
{
    enum { CHUNK = 32768 };
    static wchar_t chunk[CHUNK + 1];
    size_t n;
    AcquireSRWLockExclusive(&lock);
    unsigned long long first = log_total - log_len;
    if (log_shown < first)
        log_shown = first;
    n = (size_t)(log_total - log_shown);
    if (n > CHUNK)
        n = CHUNK;
    memcpy(chunk, log_text + (log_shown - first), n * sizeof *chunk);
    ReleaseSRWLockExclusive(&lock);
    /* Whole lines only. */
    while (n > 0 && chunk[n - 1] != L'\n')
        n--;
    if (n == 0)
        return;
    log_shown += n;
    chunk[n] = 0;
    append_log(chunk);
}

static void copy_log(void)
{
    int len = GetWindowTextLengthW(log_box);
    HGLOBAL mem = GlobalAlloc(GMEM_MOVEABLE, ((size_t)len + 1) * sizeof(wchar_t));
    if (!mem)
        return;
    wchar_t *p = GlobalLock(mem);
    GetWindowTextW(log_box, p, len + 1);
    GlobalUnlock(mem);
    if (OpenClipboard(main_hwnd)) {
        EmptyClipboard();
        if (SetClipboardData(CF_UNICODETEXT, mem))
            mem = NULL;
        CloseClipboard();
    }
    if (mem)
        GlobalFree(mem);
}

/* ---- The window ----------------------------------------------------------- */

static void refresh(void)
{
    State st;
    unsigned fields, lists;
    wchar_t values[F_COUNT][FIELD_MAX], additions[2][FIELD_MAX];
    AcquireSRWLockExclusive(&lock);
    st = shared;
    fields = pushed_fields;
    pushed_fields = 0;
    for (int i = 0; i < F_COUNT; i++)
        if (fields & 1u << i)
            copy_text(values[i], FIELD_MAX, pushed[i]);
    lists = added_lists;
    added_lists = 0;
    memcpy(additions, added, sizeof added);
    ReleaseSRWLockExclusive(&lock);

    for (int i = 0; i < F_COUNT; i++)
        if (fields & 1u << i)
            show_field(i, values[i]);
    for (int i = 0; i < 2; i++)
        if (lists & 1u << i) {
            add_binding(i ? F_PAUSE : F_RECENTER, additions[i]);
            if (InterlockedCompareExchange(&capturing, 0, 0) == i)
                end_capture();
        }
    if (wcscmp(st.calibration, seen.calibration) != 0)
        SetWindowTextW(calibration_text, st.calibration);
    if (wcscmp(st.settings_message, seen.settings_message) != 0)
        SetWindowTextW(settings_text, st.settings_message);
    if (st.paused != seen.paused || fields & (1u << F_PAUSE)) {
        wchar_t label[FIELD_MAX + 32];
        binding_label(label, FIELD_MAX + 32, st.paused ? L"Resume" : L"Pause", field_text[F_PAUSE]);
        SetWindowTextW(pause_button, label);
    }
    seen = st;
    drain_log();
    update_tray(0);
    if (IsWindowVisible(main_hwnd) && !IsIconic(main_hwnd))
        InvalidateRect(live_hwnd, NULL, FALSE);
}

static HWND control(const wchar_t *cls, const wchar_t *text, DWORD style, int x, int y, int w, int h, int id)
{
    DWORD ex = wcscmp(cls, L"EDIT") == 0 ? WS_EX_CLIENTEDGE : 0;
    HWND c = CreateWindowExW(ex, cls, text, WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | style, S(x), S(y), S(w), S(h),
                             main_hwnd, (HMENU)(INT_PTR)id, inst, NULL);
    SendMessageW(c, WM_SETFONT, (WPARAM)ui_font, FALSE);
    return c;
}

static HWND label(const wchar_t *text, int x, int y, int w) { return control(L"STATIC", text, SS_LEFT, x, y + 3, w, 20, 0); }

static HWND edit(int field, int x, int y, int w)
{
    return control(L"EDIT", L"", WS_TABSTOP | ES_AUTOHSCROLL, x, y, w, 23, ID_FIELD + field);
}

/* Layout, in pixels at 96 dpi. The log box takes what height is left. */
#define CLIENT_W 720
#define CLIENT_H 790
#define LOG_TOP 598

static void build_window(void)
{
    live_hwnd = CreateWindowExW(0, L"trackkat_live", L"", WS_CHILD | WS_VISIBLE, S(10), S(10), S(700), S(242),
                                main_hwnd, NULL, inst, NULL);

    recenter_button = control(L"BUTTON", L"Recenter", WS_TABSTOP | BS_PUSHBUTTON, 10, 262, 150, 28, ID_RECENTER);
    pause_button = control(L"BUTTON", L"Pause", WS_TABSTOP | BS_PUSHBUTTON, 170, 262, 150, 28, ID_PAUSE);
    control(L"BUTTON", L"Calibrate level", WS_TABSTOP | BS_PUSHBUTTON, 330, 262, 150, 28, ID_CALIBRATE);
    HWND hint = control(L"STATIC", L"Closing this window keeps trackkat running in the tray.", SS_LEFT, 492, 264,
                        218, 30, 0);
    SendMessageW(hint, WM_SETFONT, (WPARAM)small_font, FALSE);

    calibration_text = control(L"STATIC", L"", SS_LEFT | SS_NOPREFIX, 22, 318, 676, 36, 0);

    for (int i = 0; i < 2; i++) {
        int y = 390 + i * 30;
        label(i ? L"Pause" : L"Recenter", 22, y, 80);
        field_ctl[F_RECENTER + i] = edit(F_RECENTER + i, 104, y, 470);
        add_button[i] = control(L"BUTTON", L"Add key or button…", WS_TABSTOP | BS_PUSHBUTTON, 580, y - 1, 120,
                                25, i ? ID_ADD_PAUSE : ID_ADD_RECENTER);
        button_proc = (WNDPROC)SetWindowLongPtrW(add_button[i], GWLP_WNDPROC, (LONG_PTR)add_button_proc);
    }

    field_ctl[F_SMOOTHING] =
        control(L"BUTTON", L"Smoothing", WS_TABSTOP | BS_AUTOCHECKBOX, 22, 456, 92, 22, ID_FIELD + F_SMOOTHING);
    label(L"Steadiness", 120, 456, 70);
    field_ctl[F_STEADINESS] = control(TRACKBAR_CLASSW, L"", WS_TABSTOP | TBS_HORZ | TBS_NOTICKS, 188, 454, 140, 26,
                                      ID_FIELD + F_STEADINESS);
    slider_value[0] = label(L"", 330, 456, 40);
    label(L"Responsiveness", 380, 456, 96);
    field_ctl[F_RESPONSIVENESS] = control(TRACKBAR_CLASSW, L"", WS_TABSTOP | TBS_HORZ | TBS_NOTICKS, 476, 454, 140,
                                          26, ID_FIELD + F_RESPONSIVENESS);
    slider_value[1] = label(L"", 618, 456, 40);
    for (int i = F_STEADINESS; i <= F_RESPONSIVENESS; i++) {
        SendMessageW(field_ctl[i], TBM_SETRANGE, FALSE, MAKELPARAM(0, 100));
        SendMessageW(field_ctl[i], TBM_SETPAGESIZE, 0, 5);
    }

    static const wchar_t *const curve_labels[6] = {L"Yaw", L"Pitch", L"Roll", L"X", L"Y", L"Z"};
    for (int i = 0; i < 6; i++) {
        int x = 22 + (i % 3) * 226, y = 492 + (i / 3) * 30;
        label(curve_labels[i], x, y, 40);
        field_ctl[F_YAW + i] = edit(F_YAW + i, x + 40, y, 170);
    }

    control(L"BUTTON", L"Save", WS_TABSTOP | BS_DEFPUSHBUTTON, 22, 554, 90, 26, IDOK);
    settings_text = control(L"STATIC", L"", SS_LEFT | SS_NOPREFIX | SS_ENDELLIPSIS, 124, 558, 578, 20, 0);

    log_hint = control(L"STATIC", L"Everything trackkat prints. Copy it into bug reports.", SS_LEFT, 22, LOG_TOP + 22,
                       460, 20, 0);
    SendMessageW(log_hint, WM_SETFONT, (WPARAM)small_font, FALSE);
    copy_button = control(L"BUTTON", L"Copy log", WS_TABSTOP | BS_PUSHBUTTON, 600, LOG_TOP + 18, 100, 24, ID_COPY_LOG);
    log_box = control(L"EDIT", L"", WS_VSCROLL | ES_MULTILINE | ES_READONLY | ES_AUTOVSCROLL, 20, LOG_TOP + 48, 680,
                      CLIENT_H - LOG_TOP - 68, 0);
    SendMessageW(log_box, WM_SETFONT, (WPARAM)mono_font, FALSE);
    SendMessageW(log_box, EM_SETLIMITTEXT, LOG_MAX * 2, 0);

    /* Group boxes last, beneath the controls they frame. */
    control(L"BUTTON", L"Level calibration", BS_GROUPBOX, 10, 298, 700, 62, 0);
    control(L"BUTTON", L"Settings", BS_GROUPBOX, 10, 368, 700, 220, 0);
    log_group = control(L"BUTTON", L"Log", BS_GROUPBOX, 10, LOG_TOP, 700, CLIENT_H - LOG_TOP - 10, 0);
}

static void layout(int width, int height)
{
    int log_h = height - S(LOG_TOP) - S(10);
    if (log_h < S(100))
        log_h = S(100);
    MoveWindow(log_group, S(10), S(LOG_TOP), width - S(20), log_h, TRUE);
    MoveWindow(copy_button, width - S(120), S(LOG_TOP + 18), S(100), S(24), TRUE);
    MoveWindow(log_box, S(20), S(LOG_TOP + 48), width - S(40), log_h - S(58), TRUE);
}

static LRESULT CALLBACK main_proc(HWND w, UINT msg, WPARAM wp, LPARAM lp)
{
    switch (msg) {
    case WM_COMMAND:
        switch (LOWORD(wp)) {
        case ID_RECENTER:
            InterlockedOr(&actions, ACT_RECENTER);
            return 0;
        case ID_PAUSE:
            InterlockedOr(&actions, ACT_PAUSE);
            return 0;
        case ID_CALIBRATE:
            InterlockedOr(&actions, ACT_CALIBRATE);
            return 0;
        case ID_ADD_RECENTER:
        case ID_ADD_PAUSE:
            start_capture(LOWORD(wp) == ID_ADD_PAUSE);
            return 0;
        case ID_COPY_LOG:
            copy_log();
            return 0;
        case ID_FIELD + F_SMOOTHING:
            show_field(F_SMOOTHING,
                       SendMessageW(field_ctl[F_SMOOTHING], BM_GETCHECK, 0, 0) == BST_CHECKED ? L"on" : L"off");
            return 0;
        case IDOK:
            save_settings();
            return 0;
        case IDCANCEL:
            ShowWindow(w, SW_HIDE);
            return 0;
        }
        break;
    case WM_HSCROLL:
        for (int i = F_STEADINESS; i <= F_RESPONSIVENESS; i++)
            if ((HWND)lp == field_ctl[i]) {
                wchar_t v[16];
                _snwprintf(v, 16, L"%d", (int)SendMessageW(field_ctl[i], TBM_GETPOS, 0, 0));
                v[15] = 0;
                show_field(i, v);
            }
        return 0;
    case WM_CTLCOLORSTATIC:
        /* Read-only, but it takes selections, so it looks like it. */
        if ((HWND)lp == log_box) {
            SetBkColor((HDC)wp, RGB(255, 255, 255));
            return (LRESULT)log_brush;
        }
        break;
    case WM_SIZE:
        layout(LOWORD(lp), HIWORD(lp));
        return 0;
    case WM_GETMINMAXINFO: {
        RECT r = {0, 0, S(CLIENT_W), S(LOG_TOP + 160)};
        AdjustWindowRect(&r, WS_OVERLAPPEDWINDOW, FALSE);
        MINMAXINFO *mm = (MINMAXINFO *)lp;
        mm->ptMinTrackSize.x = r.right - r.left;
        mm->ptMinTrackSize.y = r.bottom - r.top;
        return 0;
    }
    case WM_CLOSE:
        ShowWindow(w, SW_HIDE);
        return 0;
    }
    return DefWindowProcW(w, msg, wp, lp);
}

static LRESULT CALLBACK tray_proc(HWND w, UINT msg, WPARAM wp, LPARAM lp)
{
    if (msg == taskbar_created && taskbar_created) {
        /* Explorer restarted, and forgot the icon. */
        tray_added = 0;
        update_tray(1);
        return 0;
    }
    switch (msg) {
    case WM_TIMER:
        refresh();
        return 0;
    case WM_APP_TRAY:
        if (lp == WM_LBUTTONUP || lp == WM_LBUTTONDBLCLK)
            show_window();
        else if (lp == WM_RBUTTONUP || lp == WM_CONTEXTMENU)
            tray_menu();
        return 0;
    case WM_APP_SHOW:
        show_window();
        return 0;
    case WM_COMMAND:
        switch (LOWORD(wp)) {
        case ID_MENU_OPEN:
            show_window();
            break;
        case ID_MENU_RECENTER:
            InterlockedOr(&actions, ACT_RECENTER);
            break;
        case ID_MENU_PAUSE:
            InterlockedOr(&actions, ACT_PAUSE);
            break;
        case ID_MENU_QUIT:
            InterlockedOr(&actions, ACT_QUIT);
            break;
        }
        return 0;
    }
    return DefWindowProcW(w, msg, wp, lp);
}

/* Use version 6 of the common controls, for buttons and sliders that look
 * like the rest of Windows, through the manifest Windows keeps in shell32
 * for this; without it they look like Windows 95. */
static void use_modern_controls(void)
{
    wchar_t shell32[MAX_PATH];
    UINT n = GetSystemDirectoryW(shell32, MAX_PATH);
    if (n == 0 || n + 13 >= MAX_PATH)
        return;
    wcscat(shell32, L"\\shell32.dll");
    ACTCTXW ctx;
    memset(&ctx, 0, sizeof ctx);
    ctx.cbSize = sizeof ctx;
    ctx.dwFlags = ACTCTX_FLAG_RESOURCE_NAME_VALID;
    ctx.lpSource = shell32;
    ctx.lpResourceName = MAKEINTRESOURCEW(124);
    HANDLE h = CreateActCtxW(&ctx);
    ULONG_PTR cookie;
    if (h != INVALID_HANDLE_VALUE)
        ActivateActCtx(h, &cookie);
}

static void init_common_controls(void)
{
    typedef BOOL(WINAPI * init_fn)(const INITCOMMONCONTROLSEX *);
    HMODULE comctl = LoadLibraryW(L"comctl32.dll");
    init_fn init = comctl ? (init_fn)(void *)GetProcAddress(comctl, "InitCommonControlsEx") : NULL;
    INITCOMMONCONTROLSEX icc = {sizeof icc, ICC_BAR_CLASSES | ICC_STANDARD_CLASSES};
    if (init)
        init(&icc);
}

static void make_fonts(void)
{
    NONCLIENTMETRICSW ncm;
    memset(&ncm, 0, sizeof ncm);
    ncm.cbSize = sizeof ncm;
    LOGFONTW lf;
    if (SystemParametersInfoW(SPI_GETNONCLIENTMETRICS, sizeof ncm, &ncm, 0))
        lf = ncm.lfMessageFont;
    else {
        memset(&lf, 0, sizeof lf);
        lf.lfHeight = -S(12);
        wcscpy(lf.lfFaceName, L"Segoe UI");
    }
    ui_font = CreateFontIndirectW(&lf);
    LOGFONTW b = lf;
    b.lfWeight = FW_BOLD;
    bold_font = CreateFontIndirectW(&b);
    LOGFONTW s = lf;
    s.lfHeight = lf.lfHeight * 9 / 10;
    small_font = CreateFontIndirectW(&s);
    LOGFONTW m = lf;
    m.lfPitchAndFamily = FIXED_PITCH | FF_MODERN;
    wcscpy(m.lfFaceName, L"Consolas");
    mono_font = CreateFontIndirectW(&m);
    log_brush = CreateSolidBrush(RGB(255, 255, 255));
}

static int create_windows(void)
{
    inst = GetModuleHandleW(NULL);
    HDC screen = GetDC(NULL);
    dpi = GetDeviceCaps(screen, LOGPIXELSY);
    ReleaseDC(NULL, screen);
    use_modern_controls();
    init_common_controls();
    make_fonts();

    WNDCLASSW wc;
    memset(&wc, 0, sizeof wc);
    wc.hInstance = inst;
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    wc.lpfnWndProc = tray_proc;
    wc.lpszClassName = L"trackkat";
    if (!RegisterClassW(&wc))
        return 0;
    wc.lpfnWndProc = main_proc;
    wc.hbrBackground = (HBRUSH)(COLOR_BTNFACE + 1);
    wc.lpszClassName = L"trackkat_window";
    if (!RegisterClassW(&wc))
        return 0;
    wc.lpfnWndProc = live_proc;
    wc.hbrBackground = NULL;
    wc.lpszClassName = L"trackkat_live";
    if (!RegisterClassW(&wc))
        return 0;

    /* Hidden, but top-level, so a second trackkat can find it and Explorer's
     * TaskbarCreated reaches it. */
    tray_hwnd = CreateWindowExW(0, L"trackkat", L"trackkat", WS_OVERLAPPED, 0, 0, 0, 0, NULL, NULL, inst, NULL);
    if (!tray_hwnd)
        return 0;
    RECT r = {0, 0, S(CLIENT_W), S(CLIENT_H)};
    AdjustWindowRect(&r, WS_OVERLAPPEDWINDOW, FALSE);
    main_hwnd = CreateWindowExW(WS_EX_CONTROLPARENT, L"trackkat_window", L"trackkat",
                                WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT,
                                r.right - r.left, r.bottom - r.top, NULL, NULL, inst, NULL);
    if (!main_hwnd)
        return 0;
    build_window();

    taskbar_created = RegisterWindowMessageW(L"TaskbarCreated");
    memset(&nid, 0, sizeof nid);
    nid.cbSize = sizeof nid;
    nid.hWnd = tray_hwnd;
    nid.uID = 1;
    nid.uFlags = NIF_ICON | NIF_TIP | NIF_MESSAGE;
    nid.uCallbackMessage = WM_APP_TRAY;
    refresh();
    update_tray(1);
    SetTimer(tray_hwnd, TIMER_ID, TIMER_MS, NULL);
    return 1;
}

static DWORD WINAPI gui_thread(LPVOID unused)
{
    (void)unused;
    if (!create_windows()) {
        InterlockedExchange(&gui_error, (LONG)GetLastError());
        InterlockedExchange(&gui_status, 2);
        return 0;
    }
    InterlockedExchange(&gui_status, 1);
    if (show_at_start)
        show_window();

    MSG msg;
    while (GetMessageW(&msg, NULL, 0, 0) > 0) {
        /* Gives the window Tab between controls, Enter to save and Esc to hide. */
        if (IsDialogMessageW(main_hwnd, &msg))
            continue;
        TranslateMessage(&msg);
        DispatchMessageW(&msg);
    }
    return 0;
}

/* Remove the tray icon, which Windows otherwise leaves behind until the
 * mouse passes over it. */
void hts_gui_stop(void)
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
        hts_gui_stop();
    return FALSE;
}

/* Start the window and tray icon on a thread of their own, with the window
 * shown or not. Returns 0, or a Windows error code. */
int hts_gui_start(int show)
{
    SetProcessDPIAware();
    show_at_start = show;
    SetConsoleCtrlHandler(console_closing, TRUE);
    HANDLE t = CreateThread(NULL, 0, gui_thread, NULL, 0, NULL);
    if (!t)
        return (int)GetLastError();
    CloseHandle(t);
    while (InterlockedCompareExchange(&gui_status, 0, 0) == 0)
        Sleep(1);
    return gui_status == 1 ? 0 : (int)gui_error;
}

/* Remove the tray icon and end the process at once. */
void hts_gui_quit(void)
{
    hts_gui_stop();
    ExitProcess(0);
}

/* Nonzero while the window is open; hidden to the tray doesn't count. */
int hts_gui_window_open(void)
{
    return main_hwnd && IsWindowVisible(main_hwnd);
}

/* Show the window of a trackkat already running, if there is one. Returns
 * nonzero if one was found. */
int hts_gui_show_running(void)
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
