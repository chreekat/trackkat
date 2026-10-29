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
#define ACT_TRIM 0x80
#define ACT_KEY_TRIM 0x100
#define ACT_CLEAR_CALIBRATION 0x200
#define ACT_OFFER 0x400 /* take up what the strip offers: install, or restart to update */
#define ACT_CHECK_UPDATES 0x800

/* The camera's state, as hts_gui_camera takes it. */
enum { CAMERA_SEARCHING, CAMERA_TRACKING, CAMERA_DOWN };

#define TEXT_MAX 512
#define FIELD_MAX 256

/* The settings the window edits, by their names in trackkat.conf. The
 * binding lists come first, in the order hts_gui_capturing numbers them. */
enum { F_RECENTER, F_PAUSE, F_TRIM, F_SMOOTHING, F_STEADINESS, F_RESPONSIVENESS, F_YAW, F_PITCH, F_ROLL, F_X, F_Y, F_Z, F_LEAN, F_NPCLIENT, F_UPDATES, F_STARTUP, F_LIGHTS, F_COUNT };
static const wchar_t *const field_names[F_COUNT] = {L"recenter", L"pause", L"trim", L"smoothing", L"steadiness",
                                                    L"responsiveness", L"yaw", L"pitch", L"roll", L"x", L"y", L"z", L"lean",
                                                    L"npclient", L"updates", L"startup", L"ignore lights"};
#define LISTS 3 /* binding lists: recenter, pause, trim */

/* A blob as the camera saw it, and what it was taken for. */
#define BLOBS_MAX 16
enum { SPOT_REFLECTOR, SPOT_STRAY, SPOT_MASKED };
typedef struct Spot {
    float x, y; /* in the camera's image, pixels */
    float pixels;
    int role;
} Spot;

typedef struct State {
    int camera;
    wchar_t camera_text[TEXT_MAX]; /* why the camera is down */
    int blobs;                     /* -1 while the clip is in sight, else how many reflectors are */
    float head[6];                 /* yaw, pitch, roll in degrees, x, y, z in mm */
    float view[6];                 /* the same, as games see it */
    int paused;
    int games;
    wchar_t game_name[64]; /* one of the games, or nothing if unknown */
    int trimmed;
    float trim[2]; /* yaw and pitch added to the view, in degrees */
    int trim_moves; /* whether the trim moves the view, or tilts it, too */
    wchar_t stats[TEXT_MAX];
    wchar_t calibration[TEXT_MAX];
    wchar_t settings_message[TEXT_MAX];
    wchar_t offer[TEXT_MAX];       /* what the strip offers, or nothing */
    wchar_t offer_button[64];      /* its button */
    wchar_t offer_tray[128];       /* the tray menu's item for it, or nothing for none */
    int checking;                  /* whether trackkat is looking for a newer version */
    wchar_t update_status[TEXT_MAX]; /* what the last look found, in a few words */
    int up_to_date;                /* whether that is that none is newer */
    int spots;                     /* blobs in the last frame, up to BLOBS_MAX */
    Spot spot[BLOBS_MAX];
} State;

/* ---- Shared with Haskell, under the lock --------------------------------- */

static SRWLOCK lock = SRWLOCK_INIT;
static State shared = {.camera = CAMERA_SEARCHING, .settings_message = L"Changes save as you make them."};
static wchar_t version_text[64] = L"development build"; /* set before the window is made */
static int updatable; /* whether Check for updates works; set before the window is made */
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
static volatile LONG chosen_keys[LISTS]; /* modifiers << 16 | virtual key, per binding list */
static volatile LONG capturing = -1;      /* the binding list waiting for a key or button, or -1 */
static wchar_t added[LISTS][FIELD_MAX];   /* bindings to add to the lists */
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

/* The last frame's blobs: n of them, each as x, y, pixels and role
 * (SPOT_REFLECTOR and the rest). Skipped, like hts_gui_frame, while the GUI
 * is copying. */
void hts_gui_blobs(int n, const float *blobs)
{
    if (!TryAcquireSRWLockExclusive(&lock))
        return;
    shared.spots = n < BLOBS_MAX ? n : BLOBS_MAX;
    for (int i = 0; i < shared.spots; i++) {
        const float *b = blobs + 4 * i;
        shared.spot[i] = (Spot){b[0], b[1], b[2], (int)b[3]};
    }
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

void hts_gui_trim(int trimmed, float yaw, float pitch, int moves)
{
    AcquireSRWLockExclusive(&lock);
    shared.trimmed = trimmed;
    shared.trim[0] = yaw;
    shared.trim[1] = pitch;
    shared.trim_moves = moves;
    ReleaseSRWLockExclusive(&lock);
}

/* How many games are tracking, and the name of one of them, or an empty
 * name if it isn't known. */
void hts_gui_games(int games, const wchar_t *name)
{
    AcquireSRWLockExclusive(&lock);
    shared.games = games;
    copy_text(shared.game_name, 64, name);
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

/* Offer something in the strip across the top of the window, with a button
 * that takes it up and a tray menu item that does the same; an empty text
 * takes the offer back, an empty tray item leaves it out of the menu. */
void hts_gui_offer(const wchar_t *text, const wchar_t *button, const wchar_t *tray)
{
    AcquireSRWLockExclusive(&lock);
    copy_text(shared.offer, TEXT_MAX, text);
    copy_text(shared.offer_button, 64, button);
    copy_text(shared.offer_tray, 128, tray);
    ReleaseSRWLockExclusive(&lock);
}

/* What the last look for a newer version found, in a few words, for the
 * strip across the top; up_to_date if that is that none is newer. */
void hts_gui_update_status(int up_to_date, const wchar_t *text)
{
    AcquireSRWLockExclusive(&lock);
    shared.up_to_date = up_to_date;
    copy_text(shared.update_status, TEXT_MAX, text);
    ReleaseSRWLockExclusive(&lock);
}

/* Whether trackkat is looking for a newer version: the Check for updates
 * button says so, and can't be clicked until it is done. */
void hts_gui_checking(int checking)
{
    AcquireSRWLockExclusive(&lock);
    shared.checking = checking;
    ReleaseSRWLockExclusive(&lock);
}

/* The version shown in the title, the strip and the log window. Call before
 * hts_gui_start. */
void hts_gui_version(const wchar_t *text)
{
    copy_text(version_text, 64, text);
}

/* Whether this copy can update itself, so Check for updates works. Call
 * before hts_gui_start. */
void hts_gui_updatable(int can)
{
    updatable = can;
}

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

/* A setting's value as the user last asked to save it. Returns 0, leaving
 * buf alone, for a setting the window doesn't show. */
int hts_gui_saved_setting(const wchar_t *name, wchar_t *buf, int size)
{
    int i = field_index(name);
    if (i < 0)
        return 0;
    AcquireSRWLockExclusive(&lock);
    copy_text(buf, (size_t)size, saved[i]);
    ReleaseSRWLockExclusive(&lock);
    return 1;
}

/* What the user asked for since the last call, as ACT_* bits. */
int hts_gui_take_actions(void)
{
    return (int)InterlockedExchange(&actions, 0);
}

static int list_index(int which) { return which < 0 || which >= LISTS ? 0 : which; }

/* The last key pressed to add to a binding list (0 recenter, 1 pause, 2
 * trim), as modifiers << 16 | virtual key. */
int hts_gui_chosen_key(int which)
{
    return (int)InterlockedCompareExchange(&chosen_keys[list_index(which)], 0, 0);
}

/* Which binding list waits for a key or joystick button to add: 0 recenter,
 * 1 pause, 2 trim, or -1 none. */
int hts_gui_capturing(void)
{
    return (int)InterlockedCompareExchange(&capturing, 0, 0);
}

/* Add a binding to a list in the window (0 recenter, 1 pause, 2 trim), as
 * written in trackkat.conf, and stop waiting for one. */
void hts_gui_add_binding(int which, const wchar_t *binding)
{
    which = list_index(which);
    AcquireSRWLockExclusive(&lock);
    copy_text(added[which], FIELD_MAX, binding);
    added_lists |= 1u << which;
    ReleaseSRWLockExclusive(&lock);
}

/* ---- The GUI thread's own state ------------------------------------------ */

#define WM_APP_TRAY (WM_APP + 1)
#define WM_APP_SHOW (WM_APP + 2)
#define WM_APP_CAPTURE_MOUSE (WM_APP + 3) /* to an Add button, once its click is done */
#define WM_APP_QUIT (WM_APP + 4)          /* from another trackkat, uninstalling */
#define TIMER_ID 1
#define TIMER_MS 33
#define COVER_AWAY_MS 4000 /* the calibration cover closes once the mouse has been away from it this long */

enum {
    ID_RECENTER = 100,
    ID_PAUSE,
    ID_CALIBRATE,
    ID_QUIT,
    ID_CLEAR_CALIBRATION,
    ID_COPY_LOG,
    ID_ADD_RECENTER,
    ID_ADD_PAUSE,
    ID_SMOOTHING,
    ID_TRIM,
    ID_ADD_TRIM,
    ID_OFFER,
    ID_CHECK_UPDATES,
    ID_COVER,
    ID_OPEN_LOG,
    ID_CLOSE_LOG,
    ID_FIELD = 200, /* + field index */
    ID_MENU_OPEN = 300,
    ID_MENU_RECENTER,
    ID_MENU_PAUSE,
    ID_MENU_TRIM,
    ID_MENU_QUIT,
    ID_MENU_OFFER,
    ID_CURVES_AS_TEXT,
    ID_MULTIPLIER = 400, /* + axis */
    ID_STEP = 410,       /* + axis */
    ID_PRECISION = 420,  /* + axis */
};

static HINSTANCE inst;
static HWND tray_hwnd, main_hwnd, live_hwnd, strip_hwnd, log_hwnd;
/* The lit switches in the live view, in the order of the binding lists. */
static HWND switch_ctl[LISTS];
static HWND calibration_text, settings_text, log_box, log_line, aids_label;
static HWND offer_button, check_updates; /* in the strip, one at a time */
static HWND log_hint, log_version, log_copy, log_close; /* in the log window */
/* The cover over Calibrate now and Clear calibration, and what it covers. */
static HWND cover, cover_buttons[2], cover_frame;
static int cover_lifted, cover_by_keyboard;
static ULONGLONG cover_near; /* when the mouse was last near the lifted cover */
static HWND list_heading[LISTS];
static HWND field_ctl[F_COUNT];
static HWND slider_value[2]; /* steadiness and responsiveness, as numbers */
static wchar_t field_text[F_COUNT][FIELD_MAX]; /* values of the checkbox and sliders */
static HWND add_button[LISTS];
/* Each axis's curve: a plot, a multiplier with arrows to step it, a centre
 * precision, and what they mean; or, as text, the edit in field_ctl. */
static HWND curve_plot[6], multiplier_edit[6], multiplier_step[6], precision_slider[6], precision_value[6];
static HWND curve_note[6], curve_extra[6][2]; /* the labels shown only with the controls */
static HWND curves_as_text;
static WNDPROC button_proc, edit_proc;
static int swallow_key_up; /* the key that was captured is still to come up */
static State seen; /* the last snapshot */
static int dpi = 96;
static HFONT ui_font, mono_font, small_font, bold_font;
static HBRUSH white_brush;
static UINT taskbar_created;
static int tray_added;
static NOTIFYICONDATAW nid;
static int icon_shown = -1;
static wchar_t tip_shown[128];
static unsigned long long log_shown; /* log characters put in the log box, counting all ever logged */
static int scroll_pos; /* how far the window's content is scrolled up, in pixels */

static volatile LONG gui_status; /* 0 starting, 1 running, 2 failed */
static volatile LONG gui_error;
static int show_at_start;

static int S(int x) { return MulDiv(x, dpi, 96); }

/* ---- Icons ---------------------------------------------------------------- */

/* The tray icon is the cat of icons/tray.svg, its eyes glowing in a colour
 * by state: green tracking, amber with the clip out of sight, grey looking
 * for the camera, red without one. Each is a resource (icons/trackkat.rc),
 * numbered from 2 in this order; 1 is the program's own icon. */
enum { LOOK_TRACKING, LOOK_CLIP_LOST, LOOK_SEARCHING, LOOK_DOWN };
#define PROGRAM_ICON 1
#define FIRST_TRAY_ICON 2

/* An icon from the program's resources at a size in pixels, or Windows' own
 * if the build left it out. Shared: never destroyed. */
static HICON resource_icon(int id, int size)
{
    HICON icon = LoadImageW(GetModuleHandleW(NULL), MAKEINTRESOURCEW(id), IMAGE_ICON, size, size, LR_SHARED);
    return icon ? icon : LoadIconW(NULL, (LPCWSTR)IDI_APPLICATION);
}

static int current_look(const State *st)
{
    if (st->camera == CAMERA_TRACKING)
        return st->blobs < 0 ? LOOK_TRACKING : LOOK_CLIP_LOST;
    return st->camera == CAMERA_SEARCHING ? LOOK_SEARCHING : LOOK_DOWN;
}

/* ---- Tray ----------------------------------------------------------------- */

static void describe(const State *st, wchar_t *buf, size_t size)
{
    const wchar_t *what = st->camera == CAMERA_SEARCHING ? L"Looking for the camera"
                          : st->camera == CAMERA_DOWN    ? L"No camera"
                          : st->blobs < 0                ? L"Tracking"
                                                         : L"Clip out of sight";
    _snwprintf(buf, size, L"trackkat\n%ls%ls%ls%ls", what, st->games > 0 ? L"\nGame connected" : L"",
               st->paused ? L"\nPaused" : L"", st->offer_tray[0] ? L"\nUpdate ready: restart to update" : L"");
    buf[size - 1] = 0;
}

static void update_tray(int force)
{
    if (InterlockedCompareExchange(&stopping, 0, 0))
        return;
    int look = current_look(&seen);
    wchar_t tip[128];
    describe(&seen, tip, 128);
    if (!force && tray_added && look == icon_shown && wcscmp(tip, tip_shown) == 0)
        return;
    nid.hIcon = resource_icon(FIRST_TRAY_ICON + look, GetSystemMetrics(SM_CXSMICON));
    copy_text(nid.szTip, 128, tip);
    if (tray_added)
        tray_added = Shell_NotifyIconW(NIM_MODIFY, &nid);
    if (!tray_added)
        tray_added = Shell_NotifyIconW(NIM_ADD, &nid);
    icon_shown = look;
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
    AppendMenuW(m, MF_STRING, ID_MENU_TRIM, L"&Trim: make this view forward");
    AppendMenuW(m, MF_SEPARATOR, 0, NULL);
    if (seen.offer_tray[0]) {
        AppendMenuW(m, MF_STRING, ID_MENU_OFFER, seen.offer_tray);
        AppendMenuW(m, MF_SEPARATOR, 0, NULL);
    }
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

/* The camera's image, as frameWidth and frameHeight in src/TrackKat/Frame.hs. */
#define CAMERA_W 640
#define CAMERA_H 480

/* What the camera sees, small, with its top left corner at left, top: each
 * blob a dot at its place, sized by its area but half as big again to be
 * seen, coloured by what it was taken for. */
static void draw_camera(HDC dc, int left, int top, int width)
{
    int height = width * CAMERA_H / CAMERA_W;
    fill_rect(dc, left, top, left + width, top + height, C_BACK);
    HPEN pen = CreatePen(PS_SOLID, 1, C_GRID);
    HGDIOBJ old_pen = SelectObject(dc, pen), old_brush = SelectObject(dc, GetStockObject(NULL_BRUSH));
    Rectangle(dc, left, top, left + width, top + height);
    SelectObject(dc, old_pen);
    SelectObject(dc, old_brush);
    DeleteObject(pen);
    double k = (double)width / CAMERA_W;
    /* Masked lights first, so the clip shows over them. */
    for (int pass = 0; pass < 2; pass++)
        for (int i = 0; i < seen.spots; i++) {
            const Spot *b = &seen.spot[i];
            if ((b->role == SPOT_MASKED) != (pass == 0))
                continue;
            COLORREF c = b->role == SPOT_REFLECTOR ? C_ACCENT : b->role == SPOT_STRAY ? C_AMBER : C_GREY;
            int r = (int)lround(1.5 * sqrt(b->pixels / PI) * k);
            if (r < S(1))
                r = S(1);
            disc(dc, c, c, 1, left + (int)lround(b->x * k), top + (int)lround(b->y * k), r);
        }
}

/* The head as games see it: a face that turns, tilts and moves with the
 * view, a line out of the nose showing where it looks; and in its corner,
 * what the camera sees. */
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
    /* Under the head, which can pass over it. */
    if (seen.camera == CAMERA_TRACKING)
        draw_camera(dc, left + S(6), top + S(6), S(96));

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
/* The ends of the views' ranges, as yawRange and the rest in
 * src/TrackKat/Response.hs. */
static const float axis_range[6] = {180, 90, 90, 300, 300, 300};

static void format_axis(wchar_t *buf, size_t size, int axis, float value)
{
    if (axis < 3)
        _snwprintf(buf, size, L"%+.1f°", value);
    else
        _snwprintf(buf, size, L"%+.0f mm", value);
    buf[size - 1] = 0;
}

/* A light with its label and what it says, in a row of the given width. */
static void status_light(HDC dc, int x, int y, int width, COLORREF dot, const wchar_t *label, const wchar_t *value)
{
    disc(dc, dot, dot, 1, x + S(5), y + S(9), S(4));
    text_at(dc, small_font, C_DIM, x + S(16), y, S(48), S(18), DT_LEFT, label);
    text_at(dc, ui_font, C_TEXT, x + S(64), y, width - S(64), S(18), DT_LEFT, value);
}

/* The live view's layout, in pixels at 96 dpi: the head, the axes beside it,
 * then across the whole width the lights, the switches and the figures. */
#define PANEL_W 700
#define PANEL_H 296
#define HEAD_SIZE 176
#define ROW_H 24
#define LIGHTS_Y 192
#define SWITCH_Y 216
#define SWITCH_H 52
#define SWITCH_W ((PANEL_W - 16 - 20) / 3)
#define STATS_Y 274
#define TABLE_X (8 + HEAD_SIZE + 16)
#define LIGHT_W ((PANEL_W - 16) / 3)

static void paint_live(HDC dc, int width, int height)
{
    fill_rect(dc, 0, 0, width, height, C_BACK);
    SetBkMode(dc, TRANSPARENT);
    draw_head(dc, S(8), S(8), S(HEAD_SIZE));

    int x0 = S(TABLE_X);
    int tracking = seen.camera == CAMERA_TRACKING && seen.blobs < 0;
    text_at(dc, small_font, C_DIM, x0 + S(50), S(6), S(80), S(18), DT_RIGHT, L"head");
    text_at(dc, small_font, C_DIM, x0 + S(130), S(6), S(80), S(18), DT_RIGHT, L"games see");
    int bar_l = x0 + S(226), bar_r = width - S(10);
    text_at(dc, small_font, C_DIM, bar_l, S(6), bar_r - bar_l, S(18), DT_RIGHT, L"Rest the mouse on anything for help");
    for (int i = 0; i < 6; i++) {
        int y = S(26) + i * S(ROW_H);
        wchar_t buf[32];
        text_at(dc, ui_font, C_TEXT, x0, y, S(50), S(ROW_H), DT_LEFT, axis_names[i]);
        format_axis(buf, 32, i, seen.head[i]);
        text_at(dc, mono_font, tracking ? C_TEXT : C_DIM, x0 + S(50), y, S(80), S(ROW_H), DT_RIGHT, buf);
        format_axis(buf, 32, i, seen.view[i]);
        text_at(dc, mono_font, tracking || seen.paused ? C_ACCENT : C_DIM, x0 + S(130), y, S(80), S(ROW_H), DT_RIGHT,
                buf);

        int top = y + S(5), bottom = y + S(18), mid = (bar_l + bar_r) / 2, half = (bar_r - bar_l) / 2;
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

    int x = S(8), y = S(LIGHTS_Y), w = S(LIGHT_W);
    wchar_t buf[TEXT_MAX + 64];
    switch (seen.camera) {
    case CAMERA_TRACKING:
        status_light(dc, x, y, w, C_GREEN, L"Camera", L"Connected");
        break;
    case CAMERA_SEARCHING:
        status_light(dc, x, y, w, C_GREY, L"Camera", L"Looking for it…");
        break;
    default:
        status_light(dc, x, y, w, C_RED, L"Camera", seen.camera_text[0] ? seen.camera_text : L"Not found");
    }
    x += w;
    if (seen.camera != CAMERA_TRACKING)
        status_light(dc, x, y, w, C_GREY, L"Clip", L"—");
    else if (seen.blobs < 0)
        status_light(dc, x, y, w, C_GREEN, L"Clip", L"In sight");
    else {
        _snwprintf(buf, 64, L"Out of sight (%d of 3)", seen.blobs);
        buf[63] = 0;
        status_light(dc, x, y, w, C_AMBER, L"Clip", buf);
    }
    x += w;
    /* A game by its name when it is known, and how many more. */
    if (seen.games > 0 && seen.game_name[0])
        _snwprintf(buf, 128, seen.games == 1 ? L"%ls" : L"%ls and %d more", seen.game_name, seen.games - 1);
    else
        _snwprintf(buf, 128, seen.games == 1 ? L"1 connected" : L"%d connected", seen.games);
    buf[127] = 0;
    status_light(dc, x, y, w, seen.games > 0 ? C_ACCENT : C_GREY, L"Games", seen.games > 0 ? buf : L"None");

    if (seen.camera == CAMERA_TRACKING && seen.stats[0])
        text_at(dc, small_font, C_DIM, S(8), S(STATS_Y), width - S(18), S(18), DT_LEFT, seen.stats);
}

/* ---- The lit switches ----------------------------------------------------- */

/* A switch's colours: unlit, or lit amber or blue like an annunciator. */
typedef struct {
    COLORREF back, edge, name, line;
} Lamp;
static const Lamp lamp_off = {RGB(42, 47, 57), RGB(69, 76, 89), RGB(230, 233, 239), RGB(138, 146, 161)};
static const Lamp lamp_amber = {RGB(74, 54, 18), RGB(240, 180, 69), RGB(255, 210, 122), RGB(231, 194, 120)};
static const Lamp lamp_blue = {RGB(20, 42, 69), RGB(90, 169, 255), RGB(185, 220, 255), RGB(156, 200, 245)};

static COLORREF mix(COLORREF a, COLORREF b, int percent_b)
{
    int p = percent_b, q = 100 - percent_b;
    return RGB((GetRValue(a) * q + GetRValue(b) * p) / 100, (GetGValue(a) * q + GetGValue(b) * p) / 100,
               (GetBValue(a) * q + GetBValue(b) * p) / 100);
}

/* A whole number of degrees with its sign, minus as a true minus. */
static void signed_degrees(wchar_t *buf, size_t size, float x)
{
    long n = lround(x);
    _snwprintf(buf, size, n < 0 ? L"−%ld" : L"+%ld", n < 0 ? -n : n);
    buf[size - 1] = 0;
}

/* What a switch says: its name, bold, and below it its state; and how it
 * is lit. */
static void switch_words(int which, const wchar_t **name, wchar_t *line, size_t size, const Lamp **lamp)
{
    *lamp = &lamp_off;
    if (which == 0) {
        *name = L"RECENTER";
        copy_text(line, size, L"clears trim");
    } else if (which == 1) {
        *name = seen.paused ? L"PAUSED" : L"PAUSE";
        copy_text(line, size, seen.paused ? L"games see the view held" : L"output live");
        if (seen.paused)
            *lamp = &lamp_amber;
    } else {
        *name = L"TRIM";
        if (seen.trimmed) {
            wchar_t y[16], p[16];
            signed_degrees(y, 16, seen.trim[0]);
            signed_degrees(p, 16, seen.trim[1]);
            _snwprintf(line, size, L"yaw %ls pitch %ls%ls", y, p, seen.trim_moves ? L" · moved" : L"");
            line[size - 1] = 0;
            *lamp = &lamp_blue;
        } else
            copy_text(line, size, L"off");
    }
}

/* Name a switch as it stands, for screen readers, and redraw it. */
static void name_switch(int which)
{
    static const wchar_t *const action[LISTS] = {L"Recenter", L"Pause", L"Trim"};
    const wchar_t *name;
    const Lamp *lamp;
    wchar_t line[64], text[96];
    switch_words(which, &name, line, 64, &lamp);
    _snwprintf(text, 96, L"%ls: %ls", which == 1 && seen.paused ? L"Paused" : action[which], line);
    text[95] = 0;
    SetWindowTextW(switch_ctl[which], text);
    InvalidateRect(switch_ctl[which], NULL, FALSE);
}

static void draw_switch(const DRAWITEMSTRUCT *d)
{
    int which = d->hwndItem == switch_ctl[1] ? 1 : d->hwndItem == switch_ctl[2] ? 2 : 0;
    const wchar_t *name;
    const Lamp *lamp;
    wchar_t line[64];
    switch_words(which, &name, line, 64, &lamp);
    HDC dc = d->hDC;
    RECT r = d->rcItem;
    int pressed = (d->itemState & ODS_SELECTED) != 0;
    fill_rect(dc, r.left, r.top, r.right, r.bottom, C_BACK);
    COLORREF back = pressed ? mix(lamp->back, RGB(0, 0, 0), 25) : lamp->back;
    /* A rounded face, and when lit a glow inside its edge. */
    int round = S(8);
    HBRUSH brush = CreateSolidBrush(back);
    HPEN pen = CreatePen(PS_SOLID, 1, lamp->edge);
    HGDIOBJ ob = SelectObject(dc, brush), op = SelectObject(dc, pen);
    RoundRect(dc, r.left, r.top, r.right, r.bottom, round, round);
    SelectObject(dc, GetStockObject(NULL_BRUSH));
    if (lamp != &lamp_off)
        for (int i = 1; i <= S(5); i++) {
            HPEN glow = CreatePen(PS_SOLID, 1, mix(back, lamp->edge, 50 - 45 * i / (S(5) + 1)));
            SelectObject(dc, glow);
            RoundRect(dc, r.left + i, r.top + i, r.right - i, r.bottom - i, round, round);
            SelectObject(dc, pen);
            DeleteObject(glow);
        }
    SelectObject(dc, ob);
    SelectObject(dc, op);
    DeleteObject(brush);
    DeleteObject(pen);
    SetBkMode(dc, TRANSPARENT);
    int shift = pressed ? S(1) : 0, h = r.bottom - r.top, w = r.right - r.left;
    SetTextCharacterExtra(dc, S(2));
    text_at(dc, bold_font, lamp->name, r.left + shift, r.top + h / 2 - S(19) + shift, w, S(20), DT_CENTER, name);
    SetTextCharacterExtra(dc, 0);
    text_at(dc, small_font, lamp->line, r.left + S(4) + shift, r.top + h / 2 + shift, w - S(8), S(18), DT_CENTER,
            line);
    if (d->itemState & ODS_FOCUS && !(d->itemState & ODS_NOFOCUSRECT)) {
        RECT f = {r.left + S(4), r.top + S(4), r.right - S(4), r.bottom - S(4)};
        SetTextColor(dc, RGB(0, 0, 0));
        SetBkColor(dc, RGB(255, 255, 255));
        DrawFocusRect(dc, &f);
    }
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
    case WM_DRAWITEM:
        draw_switch((const DRAWITEMSTRUCT *)lp);
        return TRUE;
    case WM_COMMAND:
        /* The switches' clicks are the window's to act on. */
        return SendMessageW(main_hwnd, msg, wp, lp);
    }
    return DefWindowProcW(w, msg, wp, lp);
}

/* ---- The strip across the top --------------------------------------------- */

#define STRIP_H 40
#define STRIP_BUTTON_W 150 /* the widest of its buttons */
#define C_STRIP RGB(228, 228, 228)
#define C_STRIP_EDGE RGB(207, 207, 207)
#define C_OFFER RGB(255, 244, 214)
#define C_OFFER_EDGE RGB(230, 207, 138)
#define C_OFFER_TEXT RGB(106, 75, 0)
#define C_GOOD RGB(46, 107, 52)

/* Draw text on one line, centred in a row of height h; returns its width. */
static int text_run(HDC dc, HFONT font, COLORREF c, int x, int h, const wchar_t *s)
{
    SelectObject(dc, font);
    RECT m = {0, 0, 0, 0};
    DrawTextW(dc, s, -1, &m, DT_SINGLELINE | DT_NOPREFIX | DT_CALCRECT);
    text_at(dc, font, c, x, 0, m.right, h, DT_LEFT, s);
    return m.right;
}

/* trackkat, its version and whether it is up to date; or, with an offer,
 * the offer, on amber. The button at the right is a child of its own. */
static void paint_strip(HDC dc, int w, int h)
{
    int offer = seen.offer[0] != 0;
    fill_rect(dc, 0, 0, w, h, offer ? C_OFFER : C_STRIP);
    fill_rect(dc, 0, h - 1, w, h, offer ? C_OFFER_EDGE : C_STRIP_EDGE);
    SetBkMode(dc, TRANSPARENT);
    int size = S(16), x = S(10);
    DrawIconEx(dc, x, (h - size) / 2, resource_icon(PROGRAM_ICON, size), size, size, 0, NULL, DI_NORMAL);
    x += size + S(8);
    x += text_run(dc, bold_font, RGB(27, 27, 27), x, h, L"trackkat") + S(8);
    /* The build's number; its commit is in the title and the log window. */
    wchar_t version[64];
    copy_text(version, 64, version_text);
    wchar_t *paren = wcsstr(version, L" (");
    if (paren)
        *paren = 0;
    x += text_run(dc, ui_font, RGB(85, 85, 85), x, h, version) + S(12);
    int right = w - S(STRIP_BUTTON_W + 20);
    if (right <= x)
        return;
    if (offer) {
        /* On one line or two, in the smaller font if it needs more. */
        RECT text = {x, 0, right, h}, measure = text;
        HFONT font = ui_font;
        SelectObject(dc, font);
        DrawTextW(dc, seen.offer, -1, &measure, DT_WORDBREAK | DT_NOPREFIX | DT_CALCRECT);
        if (measure.bottom - measure.top > h - S(8)) {
            font = small_font;
            SelectObject(dc, font);
            measure = text;
            DrawTextW(dc, seen.offer, -1, &measure, DT_WORDBREAK | DT_NOPREFIX | DT_CALCRECT);
        }
        int th = measure.bottom - measure.top;
        if (th > h - S(4))
            th = h - S(4);
        text.top = (h - th) / 2;
        text.bottom = text.top + th;
        SetTextColor(dc, C_OFFER_TEXT);
        DrawTextW(dc, seen.offer, -1, &text, DT_WORDBREAK | DT_EDITCONTROL | DT_NOPREFIX | DT_END_ELLIPSIS);
        return;
    }
    const wchar_t *status = seen.checking ? L"Checking…" : seen.update_status;
    int good = !seen.checking && seen.up_to_date;
    if (!status[0])
        return;
    if (good) {
        int cy = h / 2;
        POINT tick[3] = {{x + S(1), cy}, {x + S(4), cy + S(3)}, {x + S(10), cy - S(4)}};
        HPEN pen = CreatePen(PS_SOLID, S(2), C_GOOD);
        HGDIOBJ old = SelectObject(dc, pen);
        Polyline(dc, tick, 3);
        SelectObject(dc, old);
        DeleteObject(pen);
        x += S(15);
    }
    text_at(dc, ui_font, good ? C_GOOD : RGB(85, 85, 85), x, 0, right - x, h, DT_LEFT, status);
}

static LRESULT CALLBACK strip_proc(HWND w, UINT msg, WPARAM wp, LPARAM lp)
{
    static HBRUSH quiet, offering;
    switch (msg) {
    case WM_ERASEBKGND:
        return 1;
    case WM_PRINTCLIENT: {
        /* Behind the button's rounded corners. */
        RECT rc;
        GetClientRect(w, &rc);
        paint_strip((HDC)wp, rc.right, rc.bottom);
        return 0;
    }
    case WM_PAINT: {
        PAINTSTRUCT ps;
        HDC dc = BeginPaint(w, &ps);
        RECT rc;
        GetClientRect(w, &rc);
        paint_strip(dc, rc.right, rc.bottom);
        EndPaint(w, &ps);
        return 0;
    }
    case WM_CTLCOLORBTN:
        if (!quiet) {
            quiet = CreateSolidBrush(C_STRIP);
            offering = CreateSolidBrush(C_OFFER);
        }
        return (LRESULT)(seen.offer[0] ? offering : quiet);
    case WM_COMMAND:
        return SendMessageW(main_hwnd, msg, wp, lp);
    }
    return DefWindowProcW(w, msg, wp, lp);
}

/* ---- Response curves ------------------------------------------------------ */

/* A curve as trackkat.conf writes it, read as src/TrackKat/Config.hs reads
 * it: a multiplier with a centre precision, such as 1:6 or
 * 1:6, precision 40, or other input:output points. */
enum { SHAPE_MULTIPLIER, SHAPE_POINTS, SHAPE_UNREADABLE };
#define CURVE_POINTS 64

typedef struct {
    int shape;
    double factor;    /* the multiplier, or for points the last one's output over input */
    double precision; /* 0 to 100, for a multiplier */
    int n;
    double in[CURVE_POINTS], out[CURVE_POINTS];
} Curve;

static Curve curve[6];


static void skip_spaces(const wchar_t **p)
{
    while (**p == L' ' || **p == L'\t')
        (*p)++;
}

static void read_curve(const wchar_t *text, Curve *c)
{
    memset(c, 0, sizeof *c);
    c->shape = SHAPE_UNREADABLE;
    const wchar_t *p = text;
    for (;;) {
        skip_spaces(&p);
        if (!*p || *p == L',')
            break;
        wchar_t *end;
        double a = wcstod(p, &end);
        if (end == p || *end != L':')
            return;
        p = end + 1;
        double b = wcstod(p, &end);
        if (end == p || (*end && *end != L' ' && *end != L'\t' && *end != L','))
            return;
        p = end;
        if (c->n == CURVE_POINTS || a <= (c->n ? c->in[c->n - 1] : 0))
            return;
        c->in[c->n] = a;
        c->out[c->n] = b;
        c->n++;
    }
    if (c->n == 0)
        return;
    c->factor = c->out[c->n - 1] / c->in[c->n - 1];
    if (*p == L',') {
        p++;
        skip_spaces(&p);
        if (wcsncmp(p, L"precision", 9) != 0)
            return;
        p += 9;
        if (*p != L' ' && *p != L'\t')
            return;
        wchar_t *end;
        double k = wcstod(p, &end);
        if (end == p || k < 0 || k > 100)
            return;
        p = end;
        skip_spaces(&p);
        if (*p || c->n != 1 || c->out[0] <= 0)
            return;
        c->shape = SHAPE_MULTIPLIER;
        c->precision = k;
    } else
        c->shape = c->n == 1 && c->out[0] > 0 ? SHAPE_MULTIPLIER : SHAPE_POINTS;
}

/* Where a curve takes a head movement, as applyCurve does. */
static double apply_curve(const Curve *c, int axis, double x)
{
    double a = fabs(x), s = x < 0 ? -1 : 1;
    if (c->shape == SHAPE_MULTIPLIER) {
        double range = axis_range[axis], r = range / c->factor, k = c->precision / 100;
        if (a <= r) {
            double u = a / r;
            return s * range * ((1 - k) * u + k * u * u * u);
        }
        return s * (range + (a - r) * c->factor * (1 + 2 * k));
    }
    double x0 = 0, y0 = 0;
    for (int i = 0; i < c->n; i++) {
        double x1 = c->in[i], y1 = c->out[i];
        if (a <= x1 || i == c->n - 1)
            return s * (y0 + (a - x0) * (y1 - y0) / (x1 - x0));
        x0 = x1;
        y0 = y1;
    }
    return x;
}

/* A number with no more decimals than it needs, up to two. */
static void format_number(wchar_t *buf, size_t size, double x)
{
    _snwprintf(buf, size, L"%.2f", x);
    buf[size - 1] = 0;
    wchar_t *end = buf + wcslen(buf);
    while (end > buf && end[-1] == L'0')
        *--end = 0;
    if (end > buf && end[-1] == L'.')
        *--end = 0;
}

/* What a multiplier means: the head movement that reaches the end of the
 * view's range. */
static void describe_reach(wchar_t *buf, size_t size, int axis, double factor)
{
    static const wchar_t *const what[6] = {L"turn looks behind", L"nod looks straight up", L"tilt rolls 90°",
                                           L"lean moves 300 mm", L"rise moves 300 mm", L"lean moves 300 mm"};
    double reach = axis_range[axis] / factor;
    wchar_t n[32];
    format_number(n, 32, reach < 10 ? round(reach * 10) / 10 : round(reach));
    _snwprintf(buf, size, axis < 3 ? L"%ls° %ls" : L"%ls mm %ls", n, what[axis]);
    buf[size - 1] = 0;
}

static void paint_curve(HDC dc, int axis, int w, int h)
{
    fill_rect(dc, 0, 0, w, h, C_BOX);
    SetBkMode(dc, TRANSPARENT);
    HPEN grid = CreatePen(PS_DOT, 1, C_GRID);
    HGDIOBJ old = SelectObject(dc, grid);
    MoveToEx(dc, w / 2, 0, NULL);
    LineTo(dc, w / 2, h);
    MoveToEx(dc, 0, h / 2, NULL);
    LineTo(dc, w, h / 2);
    SelectObject(dc, old);
    DeleteObject(grid);

    const Curve *c = &curve[axis];
    if (c->shape == SHAPE_UNREADABLE) {
        text_at(dc, small_font, C_AMBER, 0, 0, w, h, DT_CENTER, L"?");
        return;
    }
    /* Up, the view from the end of its range on one side to the other;
     * across, the head a quarter further than the multiplier takes to reach
     * it, so the curve's shape fills the plot whatever the multiplier. */
    double reach = 1.25 * axis_range[axis] / (c->factor > 0 ? c->factor : 1);
    double sx = w / 2.0 / reach, sy = (h / 2.0 - S(2)) / axis_range[axis];
    HRGN clip = CreateRectRgn(0, 0, w, h);
    SelectClipRgn(dc, clip);
    if (c->shape == SHAPE_MULTIPLIER && c->precision > 0) {
        /* The multiplier alone, for comparison. */
        Curve plain = *c;
        plain.precision = 0;
        POINT a = {0, (LONG)lround(h / 2.0 + apply_curve(&plain, axis, reach) * sy)};
        POINT b = {w, (LONG)lround(h / 2.0 - apply_curve(&plain, axis, reach) * sy)};
        HPEN dots = CreatePen(PS_DOT, 1, C_DIM);
        HGDIOBJ was = SelectObject(dc, dots);
        MoveToEx(dc, a.x, a.y, NULL);
        LineTo(dc, b.x, b.y);
        SelectObject(dc, was);
        DeleteObject(dots);
    }
    POINT pts[129];
    for (int i = 0; i <= 128; i++) {
        double x = reach * (i / 64.0 - 1);
        double y = apply_curve(c, axis, x) * sy;
        /* Far off the plot, GDI coordinates would overflow. */
        y = y > h ? h : y < -h ? -h : y;
        pts[i].x = (LONG)lround(w / 2.0 + x * sx);
        pts[i].y = (LONG)lround(h / 2.0 - y);
    }
    HPEN pen = CreatePen(PS_SOLID, S(2), C_ACCENT);
    old = SelectObject(dc, pen);
    Polyline(dc, pts, 129);
    SelectObject(dc, old);
    DeleteObject(pen);

    /* Where the head is now, and where the curve puts the view. */
    int live = seen.camera == CAMERA_TRACKING && seen.blobs < 0;
    double x = seen.head[axis], y = apply_curve(c, axis, x);
    double px = w / 2.0 + x * sx, py = h / 2.0 - y * sy;
    px = px < 0 ? 0 : px > w - 1 ? w - 1 : px;
    py = py < 0 ? 0 : py > h - 1 ? h - 1 : py;
    disc(dc, live ? C_HEAD_TICK : C_DIM, C_BOX, 1, (int)lround(px), (int)lround(py), S(3));
    SelectClipRgn(dc, NULL);
    DeleteObject(clip);
}

static LRESULT CALLBACK curve_proc(HWND w, UINT msg, WPARAM wp, LPARAM lp)
{
    switch (msg) {
    case WM_ERASEBKGND:
        return 1;
    case WM_PAINT: {
        PAINTSTRUCT ps;
        HDC dc = BeginPaint(w, &ps);
        RECT rc;
        GetClientRect(w, &rc);
        HDC mem = CreateCompatibleDC(dc);
        HBITMAP bmp = CreateCompatibleBitmap(dc, rc.right, rc.bottom);
        HGDIOBJ old = SelectObject(mem, bmp);
        paint_curve(mem, (int)GetWindowLongPtrW(w, GWLP_USERDATA), rc.right, rc.bottom);
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

/* What changed a curve, so that showing it leaves alone the control the
 * user is working. */
enum { FROM_TEXT, FROM_MULTIPLIER, FROM_STEP, FROM_PRECISION };

static int syncing; /* the window is changing curve controls itself */

/* Show a curve's text in its controls, its note and its plot. A curve
 * that isn't a multiplier keeps its controls at its overall slope and no
 * precision, ready to replace it. */
static void sync_curve(int axis, int from)
{
    wchar_t text[FIELD_MAX], buf[64];
    GetWindowTextW(field_ctl[F_YAW + axis], text, FIELD_MAX);
    Curve *c = &curve[axis];
    read_curve(text, c);
    syncing++;
    if (from != FROM_MULTIPLIER && c->shape != SHAPE_UNREADABLE) {
        format_number(buf, 64, c->factor);
        SetWindowTextW(multiplier_edit[axis], buf);
    }
    int precision = c->shape == SHAPE_MULTIPLIER ? (int)lround(c->precision) : 0;
    if (from != FROM_PRECISION)
        SendMessageW(precision_slider[axis], TBM_SETPOS, TRUE, precision);
    _snwprintf(buf, 64, L"%d", precision);
    SetWindowTextW(precision_value[axis], buf);
    if (c->shape == SHAPE_MULTIPLIER)
        describe_reach(buf, 64, axis, c->factor);
    else
        copy_text(buf, 64, c->shape == SHAPE_POINTS ? L"Custom curve, of points" : L"Can't read this curve");
    SetWindowTextW(curve_note[axis], buf);
    syncing--;
    InvalidateRect(curve_plot[axis], NULL, FALSE);
}

/* Replace a curve with a multiplier and precision, as the user set them. */
static void set_multiplier(int axis, double factor, int precision, int from)
{
    wchar_t number[32], text[FIELD_MAX];
    format_number(number, 32, factor);
    if (precision > 0)
        _snwprintf(text, FIELD_MAX, L"1:%ls, precision %d", number, precision);
    else
        _snwprintf(text, FIELD_MAX, L"1:%ls", number);
    text[FIELD_MAX - 1] = 0;
    syncing++;
    SetWindowTextW(field_ctl[F_YAW + axis], text);
    syncing--;
    sync_curve(axis, from);
}

/* The multiplier as typed, if it is a number above 0. */
static int typed_multiplier(int axis, double *factor)
{
    wchar_t buf[32], *end;
    GetWindowTextW(multiplier_edit[axis], buf, 32);
    double f = wcstod(buf, &end);
    while (*end == L' ')
        end++;
    if (end == buf || *end || !(f > 0) || f > 1000)
        return 0;
    *factor = f;
    return 1;
}

static int current_precision(int axis) { return (int)SendMessageW(precision_slider[axis], TBM_GETPOS, 0, 0); }

/* The arrows step the multiplier by halves, to the next half. */
static void step_multiplier(int axis, int up)
{
    double f;
    if (!typed_multiplier(axis, &f))
        f = curve[axis].shape == SHAPE_UNREADABLE ? 1 : curve[axis].factor;
    double halves = up ? floor(f * 2 + 1e-6) + 1 : ceil(f * 2 - 1e-6) - 1;
    if (halves < 1)
        halves = 1;
    set_multiplier(axis, halves / 2, current_precision(axis), FROM_STEP);
}

static void show_curves_as_text(int as_text)
{
    for (int i = 0; i < 6; i++) {
        ShowWindow(field_ctl[F_YAW + i], as_text ? SW_SHOW : SW_HIDE);
        HWND controls[] = {multiplier_edit[i], multiplier_step[i], precision_slider[i], precision_value[i],
                           curve_extra[i][0], curve_extra[i][1]};
        for (size_t j = 0; j < sizeof controls / sizeof *controls; j++)
            ShowWindow(controls[j], as_text ? SW_HIDE : SW_SHOW);
    }
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
    if (which >= 0) {
        SetWindowTextW(add_button[which], L"Add…");
        if (GetCapture() == add_button[which])
            ReleaseCapture();
    }
}

static void start_capture(int which)
{
    end_capture();
    InterlockedExchange(&capturing, which);
    SetWindowTextW(add_button[which], L"Press it now…");
    SetFocus(add_button[which]);
    /* The button lets the mouse go after reporting its click. */
    PostMessageW(add_button[which], WM_APP_CAPTURE_MOUSE, 0, 0);
}

/* Take a key or mouse button, as a virtual-key code, for the list waiting. */
static void choose_key(int which, unsigned vk)
{
    static const LONG chosen_action[LISTS] = {ACT_KEY_RECENTER, ACT_KEY_PAUSE, ACT_KEY_TRIM};
    InterlockedExchange(&chosen_keys[which], (LONG)(hts_modifiers_held() << 16 | vk));
    InterlockedOr(&actions, chosen_action[which]);
    end_capture();
}

/* An Add button, once clicked, takes the next key, or middle or side mouse
 * button over trackkat's window, pressed (and Haskell the next joystick
 * button) and adds it to its list. Clicking stops it waiting. */
static LRESULT CALLBACK add_button_proc(HWND w, UINT msg, WPARAM wp, LPARAM lp)
{
    int id = GetDlgCtrlID(w);
    int which = id == ID_ADD_PAUSE ? 1 : id == ID_ADD_TRIM ? 2 : 0;
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
            swallow_key_up = 1;
            choose_key(which, (unsigned)wp);
        }
        return 0;
    case WM_APP_CAPTURE_MOUSE:
        if (waiting)
            SetCapture(w);
        return 0;
    case WM_MBUTTONDOWN:
        if (!waiting)
            break;
        choose_key(which, VK_MBUTTON);
        return 0;
    case WM_XBUTTONDOWN:
        if (!waiting)
            break;
        choose_key(which, GET_XBUTTON_WPARAM(wp) == XBUTTON1 ? VK_XBUTTON1 : VK_XBUTTON2);
        return TRUE;
    case WM_LBUTTONDOWN:
    case WM_RBUTTONDOWN:
        if (waiting && GetCapture() == w) {
            end_capture();
            return 0;
        }
        break;
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

#define ID_AUTOSAVE 1

/* Save the settings after the given number of milliseconds without further
 * changes, so dragging a slider saves once, when it stops. */
static void save_soon(UINT ms)
{
    SetTimer(main_hwnd, ID_AUTOSAVE, ms, NULL);
}

/* A binding list as trackkat.conf writes it, separated by commas, from its
 * box, which has one to a line; commas typed there separate them too. */
static void list_from_box(int field, wchar_t *out, size_t size)
{
    wchar_t text[FIELD_MAX * 2];
    GetWindowTextW(field_ctl[field], text, FIELD_MAX * 2);
    size_t n = 0;
    out[0] = 0;
    for (const wchar_t *p = text; *p;) {
        while (*p == L' ' || *p == L'\t' || *p == L',' || *p == L'\r' || *p == L'\n')
            p++;
        const wchar_t *end = p;
        while (*end && *end != L',' && *end != L'\r' && *end != L'\n')
            end++;
        const wchar_t *last = end;
        while (last > p && (last[-1] == L' ' || last[-1] == L'\t'))
            last--;
        if (last > p) {
            int k = _snwprintf(out + n, size - n, n ? L", %.*ls" : L"%.*ls", (int)(last - p), p);
            if (k < 0) {
                out[size - 1] = 0;
                return;
            }
            n += (size_t)k;
        }
        p = end;
    }
}

/* Show a binding list, as trackkat.conf writes it, one to a line. */
static void list_to_box(int field, const wchar_t *list)
{
    wchar_t text[FIELD_MAX * 2];
    size_t n = 0;
    for (const wchar_t *p = list; *p && n + 3 < FIELD_MAX * 2;)
        if (*p == L',') {
            text[n++] = L'\r';
            text[n++] = L'\n';
            for (p++; *p == L' '; p++)
                ;
        } else
            text[n++] = *p++;
    text[n] = 0;
    SetWindowTextW(field_ctl[field], text);
}

/* Add a binding to a list, unless it is there already. */
static void add_binding(int field, const wchar_t *binding)
{
    wchar_t text[FIELD_MAX];
    list_from_box(field, text, FIELD_MAX);
    size_t blen = wcslen(binding);
    for (const wchar_t *p = text; *p;) {
        while (*p == L' ' || *p == L',')
            p++;
        const wchar_t *end = p;
        while (*end && *end != L',')
            end++;
        if ((size_t)(end - p) == blen && _wcsnicmp(p, binding, blen) == 0)
            return;
        p = end;
    }
    wchar_t joined[FIELD_MAX];
    if (!text[0])
        copy_text(joined, FIELD_MAX, binding);
    else {
        _snwprintf(joined, FIELD_MAX, L"%ls, %ls", text, binding);
        joined[FIELD_MAX - 1] = 0;
    }
    list_to_box(field, joined);
    save_soon(1);
}

/* A binding list's box takes Enter for a new line, but leaves Tab and Esc to
 * the window. */
static LRESULT CALLBACK list_box_proc(HWND w, UINT msg, WPARAM wp, LPARAM lp)
{
    if (msg == WM_GETDLGCODE) {
        LRESULT code = CallWindowProcW(edit_proc, w, msg, wp, lp) & ~(DLGC_WANTALLKEYS | DLGC_WANTTAB);
        const MSG *m = (const MSG *)lp;
        if (m && m->message == WM_KEYDOWN && m->wParam == VK_RETURN)
            code |= DLGC_WANTMESSAGE;
        return code;
    }
    return CallWindowProcW(edit_proc, w, msg, wp, lp);
}

static int slider_field(int i) { return i == F_STEADINESS || i == F_RESPONSIVENESS; }

/* Show a setting pushed from Haskell. */
static void show_field(int i, const wchar_t *value)
{
    copy_text(field_text[i], FIELD_MAX, value);
    if (i == F_LEAN)
        SendMessageW(field_ctl[i], BM_SETCHECK, wcscmp(value, L"screen") != 0 ? BST_CHECKED : BST_UNCHECKED, 0);
    else if (i == F_NPCLIENT)
        SendMessageW(field_ctl[i], BM_SETCHECK, wcscmp(value, L"trackir") == 0 ? BST_CHECKED : BST_UNCHECKED, 0);
    else if (i == F_UPDATES || i == F_STARTUP || i == F_LIGHTS)
        SendMessageW(field_ctl[i], BM_SETCHECK, wcscmp(value, L"on") == 0 ? BST_CHECKED : BST_UNCHECKED, 0);
    else if (i == F_SMOOTHING) {
        int on = wcscmp(value, L"off") != 0;
        SendMessageW(field_ctl[i], BM_SETCHECK, on ? BST_CHECKED : BST_UNCHECKED, 0);
        EnableWindow(field_ctl[F_STEADINESS], on);
        EnableWindow(field_ctl[F_RESPONSIVENESS], on);
    } else if (slider_field(i)) {
        SendMessageW(field_ctl[i], TBM_SETPOS, TRUE, (LPARAM)lround(_wtof(value)));
        SetWindowTextW(slider_value[i == F_RESPONSIVENESS], value);
    } else if (i >= F_YAW && i <= F_Z) {
        syncing++;
        SetWindowTextW(field_ctl[i], value);
        syncing--;
        sync_curve(i - F_YAW, FROM_TEXT);
    } else /* a binding list */
        list_to_box(i, value);
}

static void save_settings(void)
{
    wchar_t values[F_COUNT][FIELD_MAX];
    for (int i = 0; i < F_COUNT; i++)
        if (i == F_SMOOTHING || i == F_LEAN || i == F_NPCLIENT || i == F_UPDATES || i == F_STARTUP || i == F_LIGHTS ||
            slider_field(i))
            copy_text(values[i], FIELD_MAX, field_text[i]);
        else if (i < F_RECENTER + LISTS)
            list_from_box(i, values[i], FIELD_MAX);
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
    /* The last line, on the window's line for the log. */
    size_t end = n;
    while (end > 0 && (chunk[end - 1] == L'\n' || chunk[end - 1] == L'\r'))
        end--;
    size_t start = end;
    while (start > 0 && chunk[start - 1] != L'\n')
        start--;
    chunk[end] = 0;
    SetWindowTextW(log_line, chunk + start);
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

/* Layout, in pixels at 96 dpi, top to bottom: the strip, the live view, the
 * bindings, the response, the calibration, the log's line and the program's
 * switches. Taller than the screen, the window scrolls. */
#define CLIENT_W 720
#define PANEL_Y (STRIP_H + 10)
#define BIND_Y (PANEL_Y + PANEL_H + 8)
#define BIND_H 130
#define RESP_Y (BIND_Y + BIND_H + 8)
#define CURVES_Y (RESP_Y + 22)
#define AIDS_Y (RESP_Y + 172)
#define RESP_H 230
#define CAL_Y (RESP_Y + RESP_H + 8)
#define COVER_X 470
#define COVER_W 230
#define LOG_Y (CAL_Y + 58)
#define MODES_Y (LOG_Y + 46)
#define CLIENT_H (MODES_Y + 46)

/* A binding list's column, under its switch. */
static int column_x(int i) { return 10 + 8 + 4 + i * (SWITCH_W + 10); }

/* Move a control to a place in the layout, wherever the window is scrolled. */
static void place(HWND c, int x, int y, int w, int h)
{
    MoveWindow(c, S(x), S(y) - scroll_pos, S(w), S(h), TRUE);
}

/* ---- Scrolling ------------------------------------------------------------ */

static void scroll_to(int pos)
{
    RECT rc;
    GetClientRect(main_hwnd, &rc);
    int most = S(CLIENT_H) - rc.bottom;
    pos = pos > most ? most : pos;
    pos = pos < 0 ? 0 : pos;
    if (pos == scroll_pos)
        return;
    ScrollWindowEx(main_hwnd, 0, scroll_pos - pos, NULL, NULL, NULL, NULL, SW_SCROLLCHILDREN | SW_INVALIDATE | SW_ERASE);
    scroll_pos = pos;
    SetScrollPos(main_hwnd, SB_VERT, pos, TRUE);
}

/* Show a scroll bar if the window is shorter than its content, keeping the
 * content's width beside it. */
static void fit_scroll(void)
{
    static int fitting;
    if (fitting)
        return;
    fitting = 1;
    RECT rc;
    GetClientRect(main_hwnd, &rc);
    SCROLLINFO si;
    memset(&si, 0, sizeof si);
    si.cbSize = sizeof si;
    si.fMask = SIF_RANGE | SIF_PAGE;
    si.nMax = S(CLIENT_H) - 1;
    si.nPage = (UINT)rc.bottom;
    SetScrollInfo(main_hwnd, SB_VERT, &si, TRUE);
    scroll_to(scroll_pos);
    GetClientRect(main_hwnd, &rc);
    if (rc.right != S(CLIENT_W)) {
        RECT wr;
        GetWindowRect(main_hwnd, &wr);
        SetWindowPos(main_hwnd, NULL, 0, 0, wr.right - wr.left + S(CLIENT_W) - rc.right, wr.bottom - wr.top,
                     SWP_NOMOVE | SWP_NOZORDER | SWP_NOACTIVATE);
    }
    fitting = 0;
}

/* ---- The calibration cover ------------------------------------------------ */

/* Lift the cover over Calibrate now and Clear calibration, leaving a strip
 * to close it by, or close it again. */
static void lift_cover(int lift)
{
    HWND focus = GetFocus();
    cover_lifted = lift;
    cover_near = GetTickCount64();
    if (lift)
        place(cover, COVER_X, CAL_Y, COVER_W, 16);
    else
        place(cover, COVER_X, CAL_Y + 6, COVER_W, 38);
    for (int i = 0; i < 2; i++)
        ShowWindow(cover_buttons[i], lift ? SW_SHOWNA : SW_HIDE);
    ShowWindow(cover_frame, lift ? SW_SHOWNA : SW_HIDE);
    SetWindowTextW(cover, lift ? L"Close the cover" : L"Lift the cover over Calibrate now and Clear calibration");
    if (!lift && (focus == cover_buttons[0] || focus == cover_buttons[1]))
        SetFocus(cover);
    InvalidateRect(cover, NULL, FALSE);
}

/* Close the lifted cover once the mouse has been away from it for a while;
 * lifted from the keyboard, it stays while the focus is on it. */
static void watch_cover(void)
{
    if (!cover_lifted)
        return;
    RECT area, hinge;
    GetWindowRect(cover_frame, &area);
    GetWindowRect(cover, &hinge);
    UnionRect(&area, &area, &hinge);
    InflateRect(&area, S(8), S(8));
    POINT p;
    int close_by = GetCursorPos(&p) && PtInRect(&area, p) && IsWindowVisible(main_hwnd) && !IsIconic(main_hwnd);
    HWND focus = GetFocus();
    if (cover_by_keyboard && (focus == cover || focus == cover_buttons[0] || focus == cover_buttons[1]))
        close_by = 1;
    ULONGLONG now = GetTickCount64();
    if (close_by)
        cover_near = now;
    else if (now - cover_near >= COVER_AWAY_MS)
        lift_cover(0);
}

/* The cover: grey, hatched, hinged at its heavier edge, and labelled with
 * what it covers; lifted, a strip to close it by. */
static void draw_cover(const DRAWITEMSTRUCT *d)
{
    HDC dc = d->hDC;
    RECT r = d->rcItem;
    COLORREF face = RGB(226, 226, 226);
    fill_rect(dc, r.left, r.top, r.right, r.bottom, face);
    HBRUSH hatch = CreateHatchBrush(HS_BDIAGONAL, RGB(204, 204, 204));
    SetBkMode(dc, TRANSPARENT);
    FillRect(dc, &r, hatch);
    DeleteObject(hatch);
    HBRUSH edge = CreateSolidBrush(RGB(180, 180, 180));
    FrameRect(dc, &r, edge);
    DeleteObject(edge);
    if (cover_lifted)
        fill_rect(dc, r.left, r.bottom - S(3), r.right, r.bottom, RGB(160, 160, 160));
    else
        fill_rect(dc, r.left, r.top, r.right, r.top + S(3), RGB(160, 160, 160));

    const wchar_t *text = cover_lifted ? L"close" : L"Calibrate · Clear";
    SelectObject(dc, small_font);
    RECT m = {0, 0, 0, 0};
    DrawTextW(dc, text, -1, &m, DT_SINGLELINE | DT_NOPREFIX | DT_CALCRECT);
    int shift = d->itemState & ODS_SELECTED ? S(1) : 0;
    int chevron = S(8), gap = S(5), total = chevron + gap + m.right;
    int x = (r.left + r.right - total) / 2 + shift, cy = (r.top + r.bottom) / 2 + (cover_lifted ? -S(1) : S(1)) + shift;
    /* A patch of the face behind the label, so the hatching doesn't cross it. */
    fill_rect(dc, x - S(4), cy - m.bottom / 2, x + total + S(4), cy - m.bottom / 2 + m.bottom, face);
    int up = cover_lifted ? -1 : 1;
    POINT v[3] = {{x, cy + up * S(2)}, {x + chevron / 2, cy - up * S(2)}, {x + chevron, cy + up * S(2)}};
    HPEN pen = CreatePen(PS_SOLID, S(3) / 2 > 0 ? S(3) / 2 : 1, RGB(102, 102, 102));
    HGDIOBJ old = SelectObject(dc, pen);
    Polyline(dc, v, 3);
    SelectObject(dc, old);
    DeleteObject(pen);
    text_at(dc, small_font, RGB(85, 85, 85), x + chevron + gap, cy - m.bottom / 2, m.right + S(2), m.bottom, DT_LEFT,
            text);
    if (d->itemState & ODS_FOCUS && !(d->itemState & ODS_NOFOCUSRECT)) {
        RECT f = r;
        InflateRect(&f, -S(2), -S(2));
        if (cover_lifted)
            f.bottom -= S(2);
        else
            f.top += S(2);
        SetTextColor(dc, RGB(0, 0, 0));
        SetBkColor(dc, RGB(255, 255, 255));
        DrawFocusRect(dc, &f);
    }
}

/* ---- Headings ------------------------------------------------------------- */

static HWND calibration_title;

/* A binding list's heading, in spaced bold capitals over a thin rule; or the
 * calibration's, bold then plain. */
static void draw_heading(const DRAWITEMSTRUCT *d)
{
    HDC dc = d->hDC;
    RECT r = d->rcItem;
    FillRect(dc, &r, GetSysColorBrush(COLOR_BTNFACE));
    SetBkMode(dc, TRANSPARENT);
    if (d->hwndItem == calibration_title) {
        int x = text_run(dc, bold_font, RGB(74, 74, 74), 0, r.bottom, L"Calibration");
        text_run(dc, ui_font, RGB(95, 95, 95), x, r.bottom, L" · learnt as you play");
        return;
    }
    wchar_t text[32];
    GetWindowTextW(d->hwndItem, text, 32);
    SetTextCharacterExtra(dc, S(1));
    text_at(dc, bold_font, RGB(27, 27, 27), 0, 0, r.right, r.bottom - S(3), DT_LEFT, text);
    SetTextCharacterExtra(dc, 0);
    fill_rect(dc, 0, r.bottom - 1, r.right, r.bottom, RGB(200, 200, 200));
}

/* ---- Help ----------------------------------------------------------------- */

static HWND tips; /* the tooltips shown when the mouse rests on something */

static void make_tips(void)
{
    tips = CreateWindowExW(WS_EX_TOPMOST, TOOLTIPS_CLASSW, NULL, WS_POPUP | TTS_ALWAYSTIP | TTS_NOPREFIX,
                           CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, main_hwnd, NULL, inst, NULL);
    /* A width makes long tips wrap; long enough to read them. */
    SendMessageW(tips, TTM_SETMAXTIPWIDTH, 0, S(360));
    SendMessageW(tips, TTM_SETDELAYTIME, TTDT_AUTOPOP, 30000);
}

static void add_tip(TTTOOLINFOW *ti, const wchar_t *text)
{
    ti->cbSize = TTTOOLINFOW_V2_SIZE; /* understood by every version of the common controls */
    ti->lpszText = (wchar_t *)text;
    SendMessageW(tips, TTM_ADDTOOLW, 0, (LPARAM)ti);
}

/* Show help when the mouse rests on a control. Returns the control. */
static HWND tip(HWND ctl, const wchar_t *text)
{
    wchar_t cls[16];
    /* A label lets the mouse through to the window beneath, unless told. */
    if (GetClassNameW(ctl, cls, 16) && _wcsicmp(cls, L"Static") == 0)
        SetWindowLongPtrW(ctl, GWL_STYLE, GetWindowLongPtrW(ctl, GWL_STYLE) | SS_NOTIFY);
    TTTOOLINFOW ti;
    memset(&ti, 0, sizeof ti);
    ti.uFlags = TTF_IDISHWND | TTF_SUBCLASS;
    ti.hwnd = main_hwnd;
    ti.uId = (UINT_PTR)ctl;
    add_tip(&ti, text);
    return ctl;
}

/* Change a control's help. */
static void retip(HWND ctl, const wchar_t *text)
{
    TTTOOLINFOW ti;
    memset(&ti, 0, sizeof ti);
    ti.cbSize = TTTOOLINFOW_V2_SIZE;
    ti.uFlags = TTF_IDISHWND;
    ti.hwnd = main_hwnd;
    ti.uId = (UINT_PTR)ctl;
    ti.lpszText = (wchar_t *)text;
    SendMessageW(tips, TTM_UPDATETIPTEXTW, 0, (LPARAM)&ti);
}

/* Show help when the mouse rests on part of a window trackkat draws: its
 * left, top, right and bottom in pixels. */
static void tip_area(HWND w, UINT_PTR id, int l, int t, int r, int b, const wchar_t *text)
{
    TTTOOLINFOW ti;
    memset(&ti, 0, sizeof ti);
    ti.uFlags = TTF_SUBCLASS;
    ti.hwnd = w;
    ti.uId = id;
    SetRect(&ti.rect, l, t, r, b);
    add_tip(&ti, text);
}

/* Help for the parts of the live view, where paint_live draws them; the
 * switches have their own. */
static void live_tips(void)
{
    int x0 = S(TABLE_X), bar_l = x0 + S(226), right = S(PANEL_W - 10);
    int rows = S(26) + 6 * S(ROW_H), lights = S(LIGHTS_Y), w = S(LIGHT_W);
    tip_area(live_hwnd, 1, S(8), S(8), S(8 + HEAD_SIZE), S(8 + HEAD_SIZE),
             L"Your head as games see it, turned and moved as the game's view will be. The blue line shows where "
             L"it looks. In the corner, what the camera sees: the clip's reflectors blue, other bright things "
             L"(ignored) amber, and still lights ignored grey.");
    tip_area(live_hwnd, 2, x0, 0, x0 + S(130), rows,
             L"Your head as the camera measures it, from the centre you set by recentering: degrees turned, mm "
             L"moved.");
    tip_area(live_hwnd, 3, x0 + S(130), 0, bar_l - S(4), rows,
             L"What games get: your head after smoothing, the curves and any trim. While paused, or with the clip "
             L"out of sight, it holds still.");
    tip_area(live_hwnd, 4, bar_l, 0, right, rows,
             L"Blue: where games see each axis, out to the end of its range (behind you, straight up, 300 mm). "
             L"Yellow tick: your head.");
    tip_area(live_hwnd, 5, S(8), lights, S(8) + w, lights + S(18),
             L"Green when trackkat has the camera; grey while it looks for it; red, with the reason, when it can't "
             L"use it. trackkat picks it up again after unplugging or sleep.");
    tip_area(live_hwnd, 6, S(8) + w, lights, S(8) + 2 * w, lights + S(18),
             L"Whether the camera sees all three reflectors on the TrackClip. Out of sight, games see your head "
             L"held where it was until it comes back.");
    tip_area(live_hwnd, 7, S(8) + 2 * w, lights, right, lights + S(18),
             L"Games reading your head through trackkat's NPClient, by the name of the game's program when trackkat "
             L"can read it. The camera's second green light shows this too.");
    tip_area(live_hwnd, 8, S(8), S(STATS_Y), right, S(PANEL_H),
             L"Over the last second: how long trackkat took from a camera frame to games having the pose (average "
             L"and worst), the longest wait between frames, the most stripes in a frame and garbage collections. "
             L"For bug reports.");
}

/* What the strip's help says: about the offer, when it makes one. */
static void tip_strip(void)
{
    static const wchar_t quiet[] =
        L"trackkat's version, and what the last look for a newer version found. An installed trackkat looks when "
        L"it starts and every few hours while Keep up to date is on; the log says more.";
    static const wchar_t install[] =
        L"trackkat runs from wherever you unzipped it until you install it. Installed, it lives in your user's "
        L"programs folder (no admin needed), starts from the Start menu, keeps itself up to date, and can be "
        L"uninstalled from Settings' apps. Settings stay where they are.";
    static const wchar_t update[] =
        L"The newer version is fetched already: Restart to update switches to it now, or it starts the next time "
        L"trackkat does. Your settings and calibration stay.";
    static wchar_t text[TEXT_MAX + 512];
    if (seen.offer[0])
        _snwprintf(text, TEXT_MAX + 512, L"%ls\n\n%ls", seen.offer, seen.offer_tray[0] ? update : install);
    else
        copy_text(text, TEXT_MAX + 512, quiet);
    text[TEXT_MAX + 511] = 0;
    retip(strip_hwnd, text);
}

/* What a binding list's heading, box and button say. */
static const wchar_t *const list_tips[LISTS] = {
    L"Keys and buttons that recenter, one to a line, such as F12, Ctrl+Mouse4 or Throttle - HOTAS Warthog button "
    L"23. They work from anywhere, even inside a game. Click the list to edit it; delete a line to remove one.",
    L"Keys and buttons that pause, one to a line, such as F9, Mouse5 or Joystick - HOTAS Warthog button 5. They "
    L"work from anywhere, even inside a game. Click the list to edit it; delete a line to remove one.",
    L"Keys and buttons that trim, one to a line, such as Ctrl+F12 or Throttle - HOTAS Warthog button 24. Empty, as "
    L"it starts, trim has no key. Click the list to edit it; delete a line to remove one."};
static const wchar_t *const add_button_tip =
    L"Click, then press the key, joystick button, or middle or side mouse button (over this window) to add. Click "
    L"again to stop waiting.";

/* ---- The window's parts --------------------------------------------------- */

/* Show the strip's offer, or its button to check for updates. */
static void show_offer(void)
{
    int offer = seen.offer[0] != 0;
    HWND focus = GetFocus();
    SetWindowTextW(offer_button, seen.offer_button);
    ShowWindow(offer_button, offer ? SW_SHOWNA : SW_HIDE);
    ShowWindow(check_updates, offer ? SW_HIDE : SW_SHOWNA);
    if (focus == (offer ? check_updates : offer_button))
        SetFocus(offer ? offer_button : check_updates);
    tip_strip();
}

static void show_log(void)
{
    if (IsIconic(log_hwnd))
        ShowWindow(log_hwnd, SW_RESTORE);
    else
        ShowWindow(log_hwnd, SW_SHOW);
    SetForegroundWindow(log_hwnd);
}

static void refresh(void)
{
    State st;
    unsigned fields, lists;
    wchar_t values[F_COUNT][FIELD_MAX], additions[LISTS][FIELD_MAX];
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
    for (int i = 0; i < LISTS; i++)
        if (lists & 1u << i) {
            add_binding(F_RECENTER + i, additions[i]);
            if (InterlockedCompareExchange(&capturing, 0, 0) == i)
                end_capture();
        }
    if (wcscmp(st.calibration, seen.calibration) != 0)
        SetWindowTextW(calibration_text, st.calibration);
    if (wcscmp(st.settings_message, seen.settings_message) != 0)
        SetWindowTextW(settings_text, st.settings_message);
    if (st.checking != seen.checking) {
        SetWindowTextW(check_updates, st.checking ? L"Checking…" : L"Check for updates");
        EnableWindow(check_updates, updatable && !st.checking);
    }
    int offer_changed = wcscmp(st.offer, seen.offer) != 0 || wcscmp(st.offer_button, seen.offer_button) != 0 ||
                        wcscmp(st.offer_tray, seen.offer_tray) != 0;
    int strip_changed = offer_changed || st.checking != seen.checking || st.up_to_date != seen.up_to_date ||
                        wcscmp(st.update_status, seen.update_status) != 0;
    int paused_changed = st.paused != seen.paused;
    int trim_changed = st.trimmed != seen.trimmed || st.trim[0] != seen.trim[0] || st.trim[1] != seen.trim[1] ||
                       st.trim_moves != seen.trim_moves;
    seen = st;
    if (paused_changed)
        name_switch(1);
    if (trim_changed)
        name_switch(2);
    if (offer_changed)
        show_offer();
    if (strip_changed)
        InvalidateRect(strip_hwnd, NULL, FALSE);
    drain_log();
    update_tray(0);
    watch_cover();
    if (IsWindowVisible(main_hwnd) && !IsIconic(main_hwnd)) {
        InvalidateRect(live_hwnd, NULL, FALSE);
        for (int i = 0; i < 6; i++)
            InvalidateRect(curve_plot[i], NULL, FALSE);
    }
}

static HWND control_in(HWND parent, const wchar_t *cls, const wchar_t *text, DWORD style, int x, int y, int w, int h,
                       int id)
{
    DWORD ex = wcscmp(cls, L"EDIT") == 0 ? WS_EX_CLIENTEDGE : 0;
    HWND c = CreateWindowExW(ex, cls, text, WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | style, S(x), S(y), S(w), S(h),
                             parent, (HMENU)(INT_PTR)id, inst, NULL);
    SendMessageW(c, WM_SETFONT, (WPARAM)ui_font, FALSE);
    return c;
}

static HWND control(const wchar_t *cls, const wchar_t *text, DWORD style, int x, int y, int w, int h, int id)
{
    return control_in(main_hwnd, cls, text, style, x, y, w, h, id);
}

static HWND label(const wchar_t *text, int x, int y, int w) { return control(L"STATIC", text, SS_LEFT, x, y + 3, w, 20, 0); }

static HWND edit(int field, int x, int y, int w)
{
    return control(L"EDIT", L"", WS_TABSTOP | ES_AUTOHSCROLL, x, y, w, 23, ID_FIELD + field);
}

/* The width of some text in a font, in pixels at 96 dpi. */
static int text_width(HFONT font, const wchar_t *text)
{
    HDC dc = GetDC(main_hwnd);
    HGDIOBJ old = SelectObject(dc, font);
    SIZE extent;
    GetTextExtentPoint32W(dc, text, (int)wcslen(text), &extent);
    SelectObject(dc, old);
    ReleaseDC(main_hwnd, dc);
    return MulDiv(extent.cx, 96, dpi);
}

/* A group box, bold-captioned if it has a caption. */
static HWND group(const wchar_t *caption, int x, int y, int w, int h)
{
    HWND g = control(L"BUTTON", caption, BS_GROUPBOX, x, y, w, h, 0);
    SendMessageW(g, WM_SETFONT, (WPARAM)bold_font, FALSE);
    return g;
}

static void build_window(void)
{
    make_tips();

    /* The strip across the top, and its buttons. */
    strip_hwnd = CreateWindowExW(WS_EX_CONTROLPARENT, L"trackkat_strip", L"", WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN,
                                 0, 0, S(CLIENT_W), S(STRIP_H), main_hwnd, NULL, inst, NULL);
    tip(strip_hwnd, L"");
    check_updates = tip(control_in(strip_hwnd, L"BUTTON", L"Check for updates", WS_TABSTOP | BS_PUSHBUTTON,
                                   CLIENT_W - 10 - 130, 8, 130, 24, ID_CHECK_UPDATES),
                        L"Look for a newer version now, even with Keep up to date off, and fetch it if there is "
                        L"one. Beside it, what was found; the log says more. Only for an installed trackkat.");
    EnableWindow(check_updates, updatable);
    offer_button = tip(control_in(strip_hwnd, L"BUTTON", L"", WS_TABSTOP | BS_DEFPUSHBUTTON,
                                  CLIENT_W - 10 - STRIP_BUTTON_W, 7, STRIP_BUTTON_W, 26, ID_OFFER),
                       L"Install trackkat and switch to the installed copy, or switch to the newer version now. "
                       L"trackkat restarts in a moment; games keep their head tracking.");
    ShowWindow(offer_button, SW_HIDE);
    tip_strip();

    /* The live view, with the switches in it. */
    live_hwnd = CreateWindowExW(WS_EX_CONTROLPARENT, L"trackkat_live", L"", WS_CHILD | WS_VISIBLE | WS_CLIPCHILDREN,
                                S(10), S(PANEL_Y), S(PANEL_W), S(PANEL_H), main_hwnd, NULL, inst, NULL);
    live_tips();
    static const int switch_ids[LISTS] = {ID_RECENTER, ID_PAUSE, ID_TRIM};
    static const wchar_t *const switch_tips[LISTS] = {
        L"Make the way you face now straight ahead, and clear any trim. Its keys and buttons, below, or a game's "
        L"own recenter binding, do the same from anywhere.",
        L"Freeze the view: games see your head held still until you press it again, handy for stretching or "
        L"looking away from the screen. Lit amber while paused.",
        L"Make the view now the new forward, where it looks and where it has moved to, handy for craning toward a "
        L"console; you can still glance around from there. Lit blue while trimmed, with where Trim made the new "
        L"forward, in degrees as games see it (pitch positive down), and \"moved\" if it also moves or tilts the "
        L"view; the log has the whole of it. Recenter clears it."};
    for (int i = 0; i < LISTS; i++) {
        switch_ctl[i] = tip(control_in(live_hwnd, L"BUTTON", L"", WS_TABSTOP | BS_OWNERDRAW, 8 + i * (SWITCH_W + 10),
                                       SWITCH_Y, SWITCH_W, SWITCH_H, switch_ids[i]),
                            switch_tips[i]);
        name_switch(i);
    }

    /* Each binding list in a column under its switch. */
    static const wchar_t *const headings[LISTS] = {L"RECENTER", L"PAUSE", L"TRIM"};
    static const int add_ids[LISTS] = {ID_ADD_RECENTER, ID_ADD_PAUSE, ID_ADD_TRIM};
    int column_w = SWITCH_W - 8;
    for (int i = 0; i < LISTS; i++) {
        int x = column_x(i);
        list_heading[i] = tip(control(L"STATIC", headings[i], SS_OWNERDRAW, x, BIND_Y + 20, column_w, 20, 0),
                              list_tips[i]);
        HWND box = CreateWindowExW(0, L"EDIT", L"",
                                   WS_CHILD | WS_VISIBLE | WS_CLIPSIBLINGS | WS_TABSTOP | ES_MULTILINE |
                                       ES_AUTOVSCROLL | ES_WANTRETURN,
                                   S(x), S(BIND_Y + 44), S(column_w), S(48), main_hwnd,
                                   (HMENU)(INT_PTR)(ID_FIELD + F_RECENTER + i), inst, NULL);
        SendMessageW(box, WM_SETFONT, (WPARAM)ui_font, FALSE);
        edit_proc = (WNDPROC)SetWindowLongPtrW(box, GWLP_WNDPROC, (LONG_PTR)list_box_proc);
        field_ctl[F_RECENTER + i] = tip(box, list_tips[i]);
        add_button[i] = tip(control(L"BUTTON", L"Add…", WS_TABSTOP | BS_PUSHBUTTON, x, BIND_Y + 96, 100, 24, add_ids[i]),
                            add_button_tip);
        button_proc = (WNDPROC)SetWindowLongPtrW(add_button[i], GWLP_WNDPROC, (LONG_PTR)add_button_proc);
    }

    /* The response: whether changes saved, and the curves, on the box's
     * top edge beside its caption. */
    int caption = text_width(bold_font, L"Response");
    settings_text = tip(
        control(L"STATIC", L"", SS_LEFT | SS_NOPREFIX | SS_ENDELLIPSIS, 18 + caption + 10, RESP_Y - 4, 330, 16, 0),
        L"Settings are written to trackkat.conf, in %APPDATA%\\trackkat, as you change them, and apply at once: "
        L"switches and sliders straight away, text when you leave its box or press Enter. Only what you changed is "
        L"rewritten, so your own edits and comments there stay. This says whether the last save worked, and if "
        L"not, why.");
    SendMessageW(settings_text, WM_SETFONT, (WPARAM)small_font, FALSE);
    curves_as_text = tip(control(L"BUTTON", L"Curves as text", WS_TABSTOP | BS_AUTOCHECKBOX, 592, RESP_Y - 8, 104,
                                 20, ID_CURVES_AS_TEXT),
                         L"Edit the curves as text, as trackkat.conf has them, to give any shape by points. "
                         L"trackkat.conf explains how.");

    static const wchar_t plot_tip[] =
        L"The curve: across, your head; up, the game's view. The dot is your head now; with a precision, the "
        L"dotted line is the multiplier alone.";
    static const wchar_t multiplier_tip[] =
        L"How many times further the view turns or moves than your head; the line below says what that comes to. "
        L"The arrows step it by halves.";
    static const wchar_t precision_tip[] =
        L"Above 0, small movements are gentler, for aiming and reading gauges, and large ones steeper, so the "
        L"same head movement still reaches the end of the view.";
    static const wchar_t note_tip[] =
        L"How far your head moves for the view to reach the end of its range. A custom curve, of points, is left "
        L"as it is unless you change its multiplier or precision.";
    static const wchar_t text_tip[] =
        L"The curve as trackkat.conf writes it: a multiplier such as 1:6, perhaps with a precision as in 1:6, "
        L"precision 40, or input:output points such as 10:10 45:120, joined by straight lines from 0:0.";
    for (int i = 0; i < 6; i++) {
        int x = 22 + (i % 3) * 226, y = CURVES_Y + (i / 3) * 72, right = x + 72;
        curve_plot[i] = tip(CreateWindowExW(0, L"trackkat_curve", L"", WS_CHILD | WS_VISIBLE, S(x), S(y), S(64),
                                            S(64), main_hwnd, NULL, inst, NULL),
                            plot_tip);
        SetWindowLongPtrW(curve_plot[i], GWLP_USERDATA, i);
        HWND name = tip(label(axis_names[i], right, y, 40), multiplier_tip);
        SendMessageW(name, WM_SETFONT, (WPARAM)bold_font, FALSE);
        curve_extra[i][0] = tip(label(L"×", right + 52, y, 12), multiplier_tip);
        multiplier_edit[i] = tip(
            control(L"EDIT", L"", WS_TABSTOP | ES_AUTOHSCROLL, right + 66, y, 74, 23, ID_MULTIPLIER + i), multiplier_tip);
        multiplier_step[i] = tip(control(UPDOWN_CLASSW, L"", UDS_ALIGNRIGHT | UDS_ARROWKEYS | UDS_HOTTRACK, 0, 0, 0,
                                         0, ID_STEP + i),
                                 multiplier_tip);
        SendMessageW(multiplier_step[i], UDM_SETBUDDY, (WPARAM)multiplier_edit[i], 0);
        SendMessageW(multiplier_step[i], UDM_SETRANGE32, 0, 1000);
        SendMessageW(multiplier_step[i], UDM_SETPOS32, 0, 500);
        curve_extra[i][1] = tip(control(L"STATIC", L"Precision", SS_LEFT, right, y + 29, 52, 18, 0), precision_tip);
        SendMessageW(curve_extra[i][1], WM_SETFONT, (WPARAM)small_font, FALSE);
        precision_slider[i] = tip(control(TRACKBAR_CLASSW, L"", WS_TABSTOP | TBS_HORZ | TBS_NOTICKS, right + 48,
                                          y + 26, 72, 22, ID_PRECISION + i),
                                  precision_tip);
        SendMessageW(precision_slider[i], TBM_SETRANGE, FALSE, MAKELPARAM(0, 100));
        SendMessageW(precision_slider[i], TBM_SETPAGESIZE, 0, 10);
        precision_value[i] = tip(control(L"STATIC", L"0", SS_LEFT, right + 120, y + 29, 22, 18, 0), precision_tip);
        SendMessageW(precision_value[i], WM_SETFONT, (WPARAM)small_font, FALSE);
        field_ctl[F_YAW + i] = tip(edit(F_YAW + i, right, y + 25, 140), text_tip);
        ShowWindow(field_ctl[F_YAW + i], SW_HIDE);
        curve_note[i] = tip(
            control(L"STATIC", L"", SS_LEFT | SS_NOPREFIX | SS_ENDELLIPSIS, right, y + 49, 142, 16, 0), note_tip);
        SendMessageW(curve_note[i], WM_SETFONT, (WPARAM)small_font, FALSE);
    }
    control(L"STATIC", L"", SS_ETCHEDHORZ, 22, AIDS_Y - 8, 676, 2, 0);

    /* Aids to the response. */
    static const wchar_t smoothing_tip[] = L"Calm the view while your head is still, at the cost of a little lag when "
                                           L"you move. Off, games get every tremor.";
    static const wchar_t steadiness_tip[] = L"How still the view stays while your head is still. Raise it if the "
                                            L"view jitters; lower it if it feels floaty or drifts into place.";
    static const wchar_t responsiveness_tip[] = L"How closely the view keeps up when you move. Raise it if the view "
                                                L"lags behind your head; lower it if it jitters as you move slowly.";
    aids_label = tip(label(L"Aids", 22, AIDS_Y, 44), smoothing_tip);
    field_ctl[F_SMOOTHING] = tip(
        control(L"BUTTON", L"Smoothing", WS_TABSTOP | BS_AUTOCHECKBOX, 70, AIDS_Y, 90, 22, ID_FIELD + F_SMOOTHING),
        smoothing_tip);
    tip(label(L"Steadiness", 170, AIDS_Y, 66), steadiness_tip);
    field_ctl[F_STEADINESS] = tip(control(TRACKBAR_CLASSW, L"", WS_TABSTOP | TBS_HORZ | TBS_NOTICKS, 236, AIDS_Y - 4,
                                          120, 26, ID_FIELD + F_STEADINESS),
                                  steadiness_tip);
    slider_value[0] = tip(label(L"", 358, AIDS_Y, 30), steadiness_tip);
    tip(label(L"Responsiveness", 396, AIDS_Y, 94), responsiveness_tip);
    field_ctl[F_RESPONSIVENESS] = tip(control(TRACKBAR_CLASSW, L"", WS_TABSTOP | TBS_HORZ | TBS_NOTICKS, 490,
                                              AIDS_Y - 4, 120, 26, ID_FIELD + F_RESPONSIVENESS),
                                      responsiveness_tip);
    slider_value[1] = tip(label(L"", 612, AIDS_Y, 30), responsiveness_tip);
    for (int i = F_STEADINESS; i <= F_RESPONSIVENESS; i++) {
        SendMessageW(field_ctl[i], TBM_SETRANGE, FALSE, MAKELPARAM(0, 100));
        SendMessageW(field_ctl[i], TBM_SETPAGESIZE, 0, 5);
    }
    field_ctl[F_LEAN] = tip(control(L"BUTTON", L"Lean follows the view", WS_TABSTOP | BS_AUTOCHECKBOX, 70,
                                    AIDS_Y + 26, 160, 22, ID_FIELD + F_LEAN),
                            L"On, leaning follows the view (lean = view): looking behind you, leaning left leans out "
                            L"of the cockpit's right side. Off, leaning moves you the same way whichever way you "
                            L"look.");
    field_ctl[F_LIGHTS] = tip(
        control(L"BUTTON", L"Ignore still lights", WS_TABSTOP | BS_AUTOCHECKBOX, 246, AIDS_Y + 26, 140, 22,
                ID_FIELD + F_LIGHTS),
        L"Learn bright lights that stay put, such as a lamp or a fire behind you, and ignore them, so they don't "
        L"stop trackkat finding the clip (ignore lights = on). The log says when one is learnt or forgotten.");

    /* The calibration, and under its cover the two buttons that change it. */
    static const wchar_t calibration_tip[] =
        L"What trackkat has learnt from how you turn your head: how the camera is tipped, and where your neck "
        L"pivots. It keeps learning as you play, and keeps it in trackkat.calibration.";
    calibration_title = tip(control(L"STATIC", L"Calibration · learnt as you play", SS_OWNERDRAW, 22, CAL_Y + 2,
                                    COVER_X - 34, 18, 0),
                            calibration_tip);
    calibration_text = tip(control(L"STATIC", L"", SS_LEFT | SS_NOPREFIX, 22, CAL_Y + 21, COVER_X - 34, 32, 0),
                           calibration_tip);
    SendMessageW(calibration_text, WM_SETFONT, (WPARAM)small_font, FALSE);
    cover = tip(control(L"BUTTON", L"", WS_TABSTOP | BS_OWNERDRAW, COVER_X, CAL_Y + 6, COVER_W, 38, ID_COVER),
                L"A cover over Calibrate now and Clear calibration, so they aren't clicked by mistake. Click it, or "
                L"press Space, to lift it. It closes again when you click close, after you use one of them, or once "
                L"the mouse has been away from it for a few seconds.");
    cover_buttons[0] =
        tip(control(L"BUTTON", L"Calibrate now", WS_TABSTOP | BS_PUSHBUTTON, COVER_X + 8, CAL_Y + 22, 104, 26,
                    ID_CALIBRATE),
            L"Learn which way is up, the camera's level, now: click, then turn your head left and right for five "
            L"seconds. trackkat learns it as you play anyway, so this is mainly for after moving the camera.");
    cover_buttons[1] = tip(control(L"BUTTON", L"Clear calibration", WS_TABSTOP | BS_PUSHBUTTON, COVER_X + 116,
                                   CAL_Y + 22, 106, 26, ID_CLEAR_CALIBRATION),
                           L"Forget what trackkat has learnt and start tracking afresh: it learns again as you turn "
                           L"your head. Use it after moving the camera or bending the clip, or if turning your head "
                           L"turns the view the wrong way.");
    cover_frame = control(L"STATIC", L"", SS_ETCHEDFRAME, COVER_X, CAL_Y + 14, COVER_W, 40, 0);
    lift_cover(0);

    /* The log: its latest line, and the rest in a window of its own. */
    tip(control(L"STATIC", L"Log", SS_LEFT, 20, LOG_Y + 10, 30, 18, 0), L"The log: what trackkat reports.");
    log_line = tip(control(L"STATIC", L"", SS_LEFT | SS_NOPREFIX | SS_ENDELLIPSIS, 54, LOG_Y + 10, 450, 18, 0),
                   L"The latest line of the log. Open log shows the whole of it.");
    SendMessageW(log_line, WM_SETFONT, (WPARAM)mono_font, FALSE);
    tip(control(L"BUTTON", L"Copy log", WS_TABSTOP | BS_PUSHBUTTON, 512, LOG_Y + 6, 90, 24, ID_COPY_LOG),
        L"Copy the whole log, to paste into a bug report.");
    tip(control(L"BUTTON", L"Open log", WS_TABSTOP | BS_PUSHBUTTON, 610, LOG_Y + 6, 90, 24, ID_OPEN_LOG),
        L"Open the log in a window of its own: what trackkat reports, such as recentering, trim, calibration, "
        L"joysticks and the buttons pressed on them, settings reloads, and errors.");

    /* How trackkat runs, and the way out. */
    control(L"STATIC", L"", SS_ETCHEDHORZ, 0, MODES_Y, CLIENT_W, 2, 0);
    field_ctl[F_STARTUP] = tip(
        control(L"BUTTON", L"Start with Windows", WS_TABSTOP | BS_AUTOCHECKBOX, 14, MODES_Y + 12, 130, 22,
                ID_FIELD + F_STARTUP),
        L"Start trackkat in the tray when you log in (startup = on). Games start trackkat themselves anyway, but "
        L"in anti-cheat mode they can't: this is the way to use it without starting trackkat by hand.");
    field_ctl[F_UPDATES] = tip(
        control(L"BUTTON", L"Keep up to date", WS_TABSTOP | BS_AUTOCHECKBOX, 156, MODES_Y + 12, 116, 22,
                ID_FIELD + F_UPDATES),
        L"Once installed, look for a newer version when trackkat starts and every few hours, and fetch it in the "
        L"background (updates = on). It is used when you click Restart to update, at the top, or when trackkat "
        L"next starts. Your settings and calibration stay.");
    field_ctl[F_NPCLIENT] = tip(
        control(L"BUTTON", L"Anti-cheat mode", WS_TABSTOP | BS_AUTOCHECKBOX, 284, MODES_Y + 12, 120, 22,
                ID_FIELD + F_NPCLIENT),
        L"For games whose anti-cheat blocks trackkat's NPClient, such as Arma Reforger (npclient = trackir): games "
        L"load the NPClient of NaturalPoint's TrackIR software instead, which must be installed but not running. "
        L"Games can't start trackkat then, so start it first, or have it start with Windows. Applies to games "
        L"started after you change it.");
    tip(control(L"BUTTON", L"Quit trackkat", WS_TABSTOP | BS_PUSHBUTTON, 590, MODES_Y + 10, 116, 26, ID_QUIT),
        L"Stop tracking and quit: the camera's lights go off and trackkat leaves the tray. Closing this window "
        L"instead keeps trackkat running in the tray.");

    /* Boxes last, beneath the controls they frame. */
    group(L"Control bindings", 10, BIND_Y, 700, BIND_H);
    group(L"Response", 10, RESP_Y - 6, 700, RESP_H + 6);
    control(L"STATIC", L"", SS_ETCHEDFRAME, 10, LOG_Y, 700, 36, 0);
}

/* ---- The log window ------------------------------------------------------- */

#define LOG_W 680
#define LOG_H 460

static void layout_log(int w, int h)
{
    MoveWindow(log_hint, S(12), S(10), w - S(24), S(18), TRUE);
    MoveWindow(log_box, S(12), S(32), w - S(24), h - S(32) - S(48), TRUE);
    MoveWindow(log_version, S(12), h - S(33), w - S(240), S(18), TRUE);
    MoveWindow(log_copy, w - S(212), h - S(38), S(96), S(26), TRUE);
    MoveWindow(log_close, w - S(108), h - S(38), S(96), S(26), TRUE);
}

static void build_log_window(void)
{
    log_hint = control_in(log_hwnd, L"STATIC", L"What trackkat reports; copy it into bug reports.",
                          SS_LEFT | SS_NOPREFIX, 0, 0, 0, 0, 0);
    log_box = tip(control_in(log_hwnd, L"EDIT", L"", WS_VSCROLL | ES_MULTILINE | ES_READONLY | ES_AUTOVSCROLL, 0, 0, 0,
                             0, 0),
                  L"What trackkat reports: recentering, trim, calibration, joysticks and the buttons pressed on them, "
                  L"settings reloads, and errors.");
    SendMessageW(log_box, WM_SETFONT, (WPARAM)mono_font, FALSE);
    SendMessageW(log_box, EM_SETLIMITTEXT, LOG_MAX * 2, 0);
    wchar_t version[96];
    _snwprintf(version, 96, L"trackkat %ls", version_text);
    version[95] = 0;
    log_version = tip(control_in(log_hwnd, L"STATIC", version, SS_LEFT | SS_NOPREFIX, 0, 0, 0, 0, 0),
                      L"The version of trackkat running, as bug reports should say; the log says it too.");
    SendMessageW(log_version, WM_SETFONT, (WPARAM)small_font, FALSE);
    log_copy = tip(control_in(log_hwnd, L"BUTTON", L"Copy log", WS_TABSTOP | BS_PUSHBUTTON, 0, 0, 0, 0, ID_COPY_LOG),
                   L"Copy the whole log, to paste into a bug report.");
    log_close = tip(control_in(log_hwnd, L"BUTTON", L"Close", WS_TABSTOP | BS_PUSHBUTTON, 0, 0, 0, 0, ID_CLOSE_LOG),
                    L"Close the log window. trackkat keeps running, and the log keeps growing.");
    RECT rc;
    GetClientRect(log_hwnd, &rc);
    layout_log(rc.right, rc.bottom);
}

static LRESULT CALLBACK log_proc(HWND w, UINT msg, WPARAM wp, LPARAM lp)
{
    switch (msg) {
    case WM_COMMAND:
        switch (LOWORD(wp)) {
        case ID_COPY_LOG:
            copy_log();
            return 0;
        case ID_CLOSE_LOG:
        case IDCANCEL:
            ShowWindow(w, SW_HIDE);
            return 0;
        }
        break;
    case WM_CTLCOLORSTATIC:
        /* Read-only, but it takes selections, so it looks like it. */
        if ((HWND)lp == log_box) {
            SetBkColor((HDC)wp, RGB(255, 255, 255));
            return (LRESULT)white_brush;
        }
        break;
    case WM_SIZE:
        if (log_box)
            layout_log(LOWORD(lp), HIWORD(lp));
        return 0;
    case WM_GETMINMAXINFO: {
        MINMAXINFO *mm = (MINMAXINFO *)lp;
        mm->ptMinTrackSize.x = S(400);
        mm->ptMinTrackSize.y = S(220);
        return 0;
    }
    case WM_CLOSE:
        ShowWindow(w, SW_HIDE);
        return 0;
    }
    return DefWindowProcW(w, msg, wp, lp);
}

/* ---- The main window's messages ------------------------------------------- */

static int is_switch(HWND c) { return c && (c == switch_ctl[0] || c == switch_ctl[1] || c == switch_ctl[2]); }

static LRESULT CALLBACK main_proc(HWND w, UINT msg, WPARAM wp, LPARAM lp)
{
    switch (msg) {
    case WM_COMMAND: {
        int id = LOWORD(wp), code = HIWORD(wp);
        if (id >= ID_FIELD + F_RECENTER && id < ID_FIELD + F_RECENTER + LISTS) {
            /* White while edited; otherwise the list reads as plain text. */
            if (code == EN_SETFOCUS || code == EN_KILLFOCUS)
                InvalidateRect((HWND)lp, NULL, TRUE);
            if (code == EN_KILLFOCUS)
                save_soon(1);
            return 0;
        }
        if (id >= ID_FIELD + F_YAW && id <= ID_FIELD + F_Z) {
            if (code == EN_CHANGE && !syncing)
                sync_curve(id - ID_FIELD - F_YAW, FROM_TEXT);
            else if (code == EN_KILLFOCUS)
                save_soon(1);
            return 0;
        }
        if (id >= ID_MULTIPLIER && id < ID_MULTIPLIER + 6) {
            int axis = id - ID_MULTIPLIER;
            double f;
            if (code == EN_CHANGE && !syncing && typed_multiplier(axis, &f))
                set_multiplier(axis, f, current_precision(axis), FROM_MULTIPLIER);
            else if (code == EN_KILLFOCUS) {
                sync_curve(axis, FROM_TEXT);
                save_soon(1);
            }
            return 0;
        }
        /* Drawn buttons report a quick second click as a double click. */
        if (code != BN_CLICKED && code != BN_DOUBLECLICKED)
            break;
        switch (id) {
        case ID_CURVES_AS_TEXT:
            show_curves_as_text(SendMessageW(curves_as_text, BM_GETCHECK, 0, 0) == BST_CHECKED);
            return 0;
        case ID_RECENTER:
            InterlockedOr(&actions, ACT_RECENTER);
            return 0;
        case ID_PAUSE:
            InterlockedOr(&actions, ACT_PAUSE);
            return 0;
        case ID_TRIM:
            InterlockedOr(&actions, ACT_TRIM);
            return 0;
        case ID_COVER: {
            /* Lifted without the mouse over it, it was from the keyboard. */
            RECT r;
            POINT p;
            GetWindowRect(cover, &r);
            cover_by_keyboard = !(GetCursorPos(&p) && PtInRect(&r, p));
            lift_cover(!cover_lifted);
            return 0;
        }
        case ID_CLEAR_CALIBRATION:
            InterlockedOr(&actions, ACT_CLEAR_CALIBRATION);
            lift_cover(0);
            return 0;
        case ID_CALIBRATE:
            InterlockedOr(&actions, ACT_CALIBRATE);
            lift_cover(0);
            return 0;
        case ID_QUIT:
            InterlockedOr(&actions, ACT_QUIT);
            return 0;
        case ID_ADD_RECENTER:
            start_capture(0);
            return 0;
        case ID_ADD_PAUSE:
            start_capture(1);
            return 0;
        case ID_ADD_TRIM:
            start_capture(2);
            return 0;
        case ID_COPY_LOG:
            copy_log();
            return 0;
        case ID_OPEN_LOG:
            show_log();
            return 0;
        case ID_FIELD + F_LEAN:
            copy_text(field_text[F_LEAN], FIELD_MAX,
                      SendMessageW(field_ctl[F_LEAN], BM_GETCHECK, 0, 0) == BST_CHECKED ? L"view" : L"screen");
            save_soon(1);
            return 0;
        case ID_FIELD + F_NPCLIENT:
            copy_text(field_text[F_NPCLIENT], FIELD_MAX,
                      SendMessageW(field_ctl[F_NPCLIENT], BM_GETCHECK, 0, 0) == BST_CHECKED ? L"trackir" : L"trackkat");
            save_soon(1);
            return 0;
        case ID_FIELD + F_UPDATES:
        case ID_FIELD + F_STARTUP:
        case ID_FIELD + F_LIGHTS: {
            int i = id - ID_FIELD;
            copy_text(field_text[i], FIELD_MAX,
                      SendMessageW(field_ctl[i], BM_GETCHECK, 0, 0) == BST_CHECKED ? L"on" : L"off");
            save_soon(1);
            return 0;
        }
        case ID_OFFER:
            InterlockedOr(&actions, ACT_OFFER);
            return 0;
        case ID_CHECK_UPDATES:
            InterlockedOr(&actions, ACT_CHECK_UPDATES);
            return 0;
        case ID_FIELD + F_SMOOTHING:
            show_field(F_SMOOTHING,
                       SendMessageW(field_ctl[F_SMOOTHING], BM_GETCHECK, 0, 0) == BST_CHECKED ? L"on" : L"off");
            save_soon(1);
            return 0;
        case IDOK: {
            /* Enter on a drawn button presses it, as on any other. */
            HWND focus = GetFocus();
            if (focus == cover || is_switch(focus))
                SendMessageW(w, WM_COMMAND, MAKEWPARAM(GetDlgCtrlID(focus), BN_CLICKED), (LPARAM)focus);
            else
                save_settings();
            return 0;
        }
        case IDCANCEL:
            ShowWindow(w, SW_HIDE);
            return 0;
        }
        break;
    }
    case WM_NOTIFY: {
        NMHDR *n = (NMHDR *)lp;
        if (n->code == UDN_DELTAPOS && n->idFrom >= ID_STEP && n->idFrom < ID_STEP + 6) {
            step_multiplier((int)n->idFrom - ID_STEP, ((NMUPDOWN *)lp)->iDelta > 0);
            save_soon(500);
            return 1;
        }
        break;
    }
    case WM_HSCROLL:
        for (int i = F_STEADINESS; i <= F_RESPONSIVENESS; i++)
            if ((HWND)lp == field_ctl[i]) {
                wchar_t v[16];
                _snwprintf(v, 16, L"%d", (int)SendMessageW(field_ctl[i], TBM_GETPOS, 0, 0));
                v[15] = 0;
                show_field(i, v);
            }
        for (int i = 0; i < 6; i++)
            if ((HWND)lp == precision_slider[i] && !syncing) {
                double f = curve[i].factor > 0 ? curve[i].factor : 1;
                set_multiplier(i, f, current_precision(i), FROM_PRECISION);
            }
        save_soon(500);
        return 0;
    case WM_VSCROLL: {
        /* The multipliers' arrows send it too. */
        if (lp)
            break;
        SCROLLINFO si;
        memset(&si, 0, sizeof si);
        si.cbSize = sizeof si;
        si.fMask = SIF_ALL;
        GetScrollInfo(w, SB_VERT, &si);
        int pos = scroll_pos;
        switch (LOWORD(wp)) {
        case SB_LINEUP:
            pos -= S(24);
            break;
        case SB_LINEDOWN:
            pos += S(24);
            break;
        case SB_PAGEUP:
            pos -= (int)si.nPage;
            break;
        case SB_PAGEDOWN:
            pos += (int)si.nPage;
            break;
        case SB_THUMBTRACK:
        case SB_THUMBPOSITION:
            pos = si.nTrackPos;
            break;
        case SB_TOP:
            pos = 0;
            break;
        case SB_BOTTOM:
            pos = S(CLIENT_H);
            break;
        }
        scroll_to(pos);
        return 0;
    }
    case WM_MOUSEWHEEL:
        scroll_to(scroll_pos - GET_WHEEL_DELTA_WPARAM(wp) * S(72) / WHEEL_DELTA);
        return 0;
    case WM_TIMER:
        if (wp == ID_AUTOSAVE) {
            KillTimer(w, ID_AUTOSAVE);
            save_settings();
            return 0;
        }
        break;
    case WM_DRAWITEM: {
        const DRAWITEMSTRUCT *d = (const DRAWITEMSTRUCT *)lp;
        if (d->CtlType == ODT_BUTTON)
            draw_cover(d);
        else
            draw_heading(d);
        return TRUE;
    }
    case WM_CTLCOLORSTATIC:
        /* The calibration and the save line, quieter than the rest. */
        if ((HWND)lp == calibration_text || (HWND)lp == settings_text || (HWND)lp == aids_label) {
            SetTextColor((HDC)wp, (HWND)lp == calibration_text ? RGB(95, 95, 95) : RGB(107, 107, 107));
            SetBkColor((HDC)wp, GetSysColor(COLOR_BTNFACE));
            return (LRESULT)GetSysColorBrush(COLOR_BTNFACE);
        }
        break;
    case WM_CTLCOLOREDIT:
        for (int i = 0; i < LISTS; i++)
            if ((HWND)lp == field_ctl[F_RECENTER + i] && GetFocus() != (HWND)lp) {
                SetBkColor((HDC)wp, GetSysColor(COLOR_BTNFACE));
                return (LRESULT)GetSysColorBrush(COLOR_BTNFACE);
            }
        break;
    case WM_SIZE:
        fit_scroll();
        return 0;
    case WM_GETMINMAXINFO: {
        /* As wide as the content and its scroll bar, if it has one, and no
         * taller than the content. */
        RECT r = {0, 0, S(CLIENT_W), S(CLIENT_H)};
        AdjustWindowRect(&r, WS_OVERLAPPEDWINDOW, FALSE);
        int bar = GetWindowLongPtrW(w, GWL_STYLE) & WS_VSCROLL ? GetSystemMetrics(SM_CXVSCROLL) : 0;
        MINMAXINFO *mm = (MINMAXINFO *)lp;
        mm->ptMinTrackSize.x = mm->ptMaxTrackSize.x = r.right - r.left + bar;
        mm->ptMaxTrackSize.y = r.bottom - r.top;
        mm->ptMinTrackSize.y = S(240);
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
    case WM_APP_QUIT:
        InterlockedOr(&actions, ACT_QUIT);
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
        case ID_MENU_TRIM:
            InterlockedOr(&actions, ACT_TRIM);
            break;
        case ID_MENU_QUIT:
            InterlockedOr(&actions, ACT_QUIT);
            break;
        case ID_MENU_OFFER:
            InterlockedOr(&actions, ACT_OFFER);
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
    INITCOMMONCONTROLSEX icc = {sizeof icc, ICC_BAR_CLASSES | ICC_STANDARD_CLASSES | ICC_UPDOWN_CLASS};
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
    white_brush = CreateSolidBrush(RGB(255, 255, 255));
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
    wc.hIcon = resource_icon(PROGRAM_ICON, GetSystemMetrics(SM_CXICON));
    wc.lpszClassName = L"trackkat_window";
    if (!RegisterClassW(&wc))
        return 0;
    wc.lpfnWndProc = live_proc;
    wc.hbrBackground = NULL;
    wc.lpszClassName = L"trackkat_live";
    if (!RegisterClassW(&wc))
        return 0;
    wc.lpfnWndProc = curve_proc;
    wc.lpszClassName = L"trackkat_curve";
    if (!RegisterClassW(&wc))
        return 0;
    wc.lpfnWndProc = strip_proc;
    wc.lpszClassName = L"trackkat_strip";
    if (!RegisterClassW(&wc))
        return 0;
    wc.lpfnWndProc = log_proc;
    wc.hbrBackground = (HBRUSH)(COLOR_BTNFACE + 1);
    wc.lpszClassName = L"trackkat_log";
    if (!RegisterClassW(&wc))
        return 0;

    /* Hidden, but top-level, so a second trackkat can find it and Explorer's
     * TaskbarCreated reaches it. */
    tray_hwnd = CreateWindowExW(0, L"trackkat", L"trackkat", WS_OVERLAPPED, 0, 0, 0, 0, NULL, NULL, inst, NULL);
    if (!tray_hwnd)
        return 0;
    RECT r = {0, 0, S(CLIENT_W), S(CLIENT_H)};
    AdjustWindowRect(&r, WS_OVERLAPPEDWINDOW, FALSE);
    /* No taller than the screen; it scrolls instead. */
    RECT work;
    int height = r.bottom - r.top;
    if (SystemParametersInfoW(SPI_GETWORKAREA, 0, &work, 0) && height > work.bottom - work.top)
        height = work.bottom - work.top;
    wchar_t title[96];
    _snwprintf(title, 96, L"trackkat %ls", version_text);
    title[95] = 0;
    main_hwnd = CreateWindowExW(WS_EX_CONTROLPARENT, L"trackkat_window", title, WS_OVERLAPPEDWINDOW & ~WS_MAXIMIZEBOX,
                                CW_USEDEFAULT, CW_USEDEFAULT, r.right - r.left, height, NULL, NULL, inst, NULL);
    if (!main_hwnd)
        return 0;
    /* The title bar's and taskbar's small icon, drawn at its own size
     * rather than shrunk from the big one. */
    HICON small_icon = resource_icon(PROGRAM_ICON, GetSystemMetrics(SM_CXSMICON));
    SendMessageW(main_hwnd, WM_SETICON, ICON_SMALL, (LPARAM)small_icon);
    build_window();
    fit_scroll();

    /* The log's own window, top-level, so it can sit beside a game. */
    RECT lr = {0, 0, S(LOG_W), S(LOG_H)};
    AdjustWindowRect(&lr, WS_OVERLAPPEDWINDOW, FALSE);
    log_hwnd = CreateWindowExW(WS_EX_CONTROLPARENT, L"trackkat_log", L"trackkat log", WS_OVERLAPPEDWINDOW, CW_USEDEFAULT,
                               CW_USEDEFAULT, lr.right - lr.left, lr.bottom - lr.top, NULL, NULL, inst, NULL);
    if (!log_hwnd)
        return 0;
    SendMessageW(log_hwnd, WM_SETICON, ICON_SMALL, (LPARAM)small_icon);
    build_log_window();

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
        /* Gives the windows Tab between controls, Enter to save and Esc to hide. */
        if (IsDialogMessageW(main_hwnd, &msg) || IsDialogMessageW(log_hwnd, &msg))
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

/* Nonzero while the window or the log's is open; hidden to the tray
 * doesn't count. */
int hts_gui_window_open(void)
{
    return (main_hwnd && IsWindowVisible(main_hwnd)) || (log_hwnd && IsWindowVisible(log_hwnd));
}

/* Ask a trackkat already running, if there is one, to quit. Returns its
 * process ID, or 0 if none was found. */
int hts_gui_quit_running(void)
{
    HWND other = FindWindowW(L"trackkat", NULL);
    if (!other)
        return 0;
    DWORD pid;
    GetWindowThreadProcessId(other, &pid);
    PostMessageW(other, WM_APP_QUIT, 0, 0);
    return (int)pid;
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
