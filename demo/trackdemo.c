/* trackdemo: a cockpit view that follows the head pose NPClient delivers.
 *
 * It loads NPClient through the registry as a game does, registers as
 * profile 9999 (which gets plain data) and polls NP_GetData every frame.
 * Without NPClient, or while the status is disabled, it says so and keeps
 * trying.
 *
 * Keys: C calls NP_ReCenter, Esc quits. */

#include <windows.h>
#include <GL/gl.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

#define NP_AXIS_MAX 16383
#define PI 3.14159265358979323846
#define PROFILE_ID 9999
#define RETRY_MS 2000

/* World units are metres; the seat's eye point is this high. */
#define EYE_HEIGHT 2.0f

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

typedef int(__stdcall *get_data_fn)(tir_data_t *);
typedef int(__stdcall *get_signature_fn)(tir_signature_t *);
typedef int(__stdcall *query_version_fn)(unsigned short *);
typedef int(__stdcall *register_hwnd_fn)(HWND);
typedef int(__stdcall *register_id_fn)(unsigned short);
typedef int(__stdcall *request_data_fn)(unsigned short);
typedef int(__stdcall *void_fn)(void);
typedef BOOL(WINAPI *swap_interval_fn)(int);

static struct {
    HMODULE dll;
    get_data_fn get_data;
    void_fn recenter, stop, unregister;
    char info[1024]; /* holds a UTF-8 path of up to MAX_PATH * 3 bytes */
    ULONGLONG last_attempt;
} np;

static HWND hwnd;
static HDC hdc;
static int width = 1280, height = 720;
static GLuint font_base, world_list;
static ULONGLONG recenter_shown_at;
static int vsync;

static struct {
    int have;   /* NP_GetData has been called */
    int ok;     /* status ok */
    short frame;
    float yaw, pitch, roll; /* degrees */
    float x, y, z;          /* mm */
    unsigned cksum_ok, cksum_bad;
} pose;

static unsigned cksum(unsigned char buf[], unsigned size)
{
    int rounds = size >> 2;
    int c = size;
    int a0, a2;
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
    c ^= (c << 3);
    c += (c >> 5);
    c ^= (c << 4);
    c += (c >> 17);
    c ^= (c << 25);
    c += (c >> 6);
    return (unsigned)c;
}

/* --- NPClient ----------------------------------------------------------- */

static void np_unload(void)
{
    if (np.dll)
        FreeLibrary(np.dll);
    np.dll = NULL;
    np.get_data = NULL;
}

static void np_try_load(void)
{
    np.last_attempt = GetTickCount64();
    wchar_t dir[MAX_PATH];
    DWORD size = sizeof dir;
    if (RegGetValueW(HKEY_CURRENT_USER, L"Software\\NaturalPoint\\NATURALPOINT\\NPClient Location", L"Path",
                     RRF_RT_REG_SZ, NULL, dir, &size) != ERROR_SUCCESS) {
        snprintf(np.info, sizeof np.info, "NPClient not found: no NPClient Location in the registry (run trackkat once)");
        return;
    }
    wchar_t path[MAX_PATH];
    _snwprintf(path, MAX_PATH, L"%ls%ls", dir, sizeof(void *) == 8 ? L"NPClient64.dll" : L"NPClient.dll");
    path[MAX_PATH - 1] = 0;
    char path8[MAX_PATH * 3];
    WideCharToMultiByte(CP_UTF8, 0, path, -1, path8, sizeof path8, NULL, NULL);
    np.dll = LoadLibraryW(path);
    if (!np.dll) {
        snprintf(np.info, sizeof np.info, "NPClient not loaded: %s (error %lu)", path8, GetLastError());
        return;
    }

    np.get_data = (get_data_fn)(void *)GetProcAddress(np.dll, "NP_GetData");
    get_signature_fn get_signature = (get_signature_fn)(void *)GetProcAddress(np.dll, "NP_GetSignature");
    query_version_fn query_version = (query_version_fn)(void *)GetProcAddress(np.dll, "NP_QueryVersion");
    register_hwnd_fn register_hwnd = (register_hwnd_fn)(void *)GetProcAddress(np.dll, "NP_RegisterWindowHandle");
    register_id_fn register_id = (register_id_fn)(void *)GetProcAddress(np.dll, "NP_RegisterProgramProfileID");
    request_data_fn request_data = (request_data_fn)(void *)GetProcAddress(np.dll, "NP_RequestData");
    void_fn start = (void_fn)(void *)GetProcAddress(np.dll, "NP_StartDataTransmission");
    np.stop = (void_fn)(void *)GetProcAddress(np.dll, "NP_StopDataTransmission");
    np.unregister = (void_fn)(void *)GetProcAddress(np.dll, "NP_UnregisterWindowHandle");
    np.recenter = (void_fn)(void *)GetProcAddress(np.dll, "NP_ReCenter");
    if (!np.get_data || !get_signature || !query_version || !register_hwnd || !register_id || !request_data ||
        !start || !np.stop || !np.unregister || !np.recenter) {
        snprintf(np.info, sizeof np.info, "NPClient is missing an export: %s", path8);
        np_unload();
        return;
    }

    tir_signature_t sig;
    memset(&sig, 0, sizeof sig);
    get_signature(&sig);
    unsigned short version = 0;
    query_version(&version);
    register_hwnd(hwnd);
    register_id(PROFILE_ID);
    request_data(0x77);
    start();
    snprintf(np.info, sizeof np.info, "NPClient %s, version 0x%04x", path8, version);
}

static void np_poll(void)
{
    if (!np.dll) {
        if (GetTickCount64() - np.last_attempt >= RETRY_MS)
            np_try_load();
        if (!np.dll)
            return;
    }
    tir_data_t d;
    np.get_data(&d);
    unsigned want = d.cksum;
    d.cksum = 0;
    if (cksum((unsigned char *)&d, sizeof d) == want)
        pose.cksum_ok++;
    else
        pose.cksum_bad++;
    pose.have = 1;
    pose.ok = d.status == 0;
    pose.frame = d.frame;
    pose.yaw = (float)(d.yaw * 180.0 / NP_AXIS_MAX);
    pose.pitch = (float)(d.pitch * 180.0 / NP_AXIS_MAX);
    pose.roll = (float)(d.roll * 180.0 / NP_AXIS_MAX);
    pose.x = (float)(d.tx * 500.0 / NP_AXIS_MAX);
    pose.y = (float)(d.ty * 500.0 / NP_AXIS_MAX);
    pose.z = (float)(d.tz * 500.0 / NP_AXIS_MAX);
}

static void np_shutdown(void)
{
    if (!np.dll)
        return;
    np.stop();
    np.unregister();
    np_unload();
}

/* --- Drawing helpers ---------------------------------------------------- */

static void shade(const float c[3], float k)
{
    glColor3f(c[0] * k, c[1] * k, c[2] * k);
}

static void quad(float ax, float ay, float az, float bx, float by, float bz, float cx, float cy, float cz, float dx,
                 float dy, float dz)
{
    glVertex3f(ax, ay, az);
    glVertex3f(bx, by, bz);
    glVertex3f(cx, cy, cz);
    glVertex3f(dx, dy, dz);
}

/* An axis-aligned box, its faces shaded differently so its shape reads. */
static void box(float x0, float y0, float z0, float x1, float y1, float z1, const float c[3])
{
    glBegin(GL_QUADS);
    shade(c, 1.0f);
    quad(x0, y1, z0, x1, y1, z0, x1, y1, z1, x0, y1, z1);
    shade(c, 0.5f);
    quad(x0, y0, z0, x1, y0, z0, x1, y0, z1, x0, y0, z1);
    shade(c, 0.85f);
    quad(x0, y0, z1, x1, y0, z1, x1, y1, z1, x0, y1, z1);
    quad(x0, y0, z0, x1, y0, z0, x1, y1, z0, x0, y1, z0);
    shade(c, 0.65f);
    quad(x0, y0, z0, x0, y0, z1, x0, y1, z1, x0, y1, z0);
    quad(x1, y0, z0, x1, y0, z1, x1, y1, z1, x1, y1, z0);
    glEnd();
}

static void pyramid(float x, float z, float half, float h, const float c[3])
{
    glBegin(GL_TRIANGLES);
    float k[4] = {0.9f, 0.7f, 0.55f, 0.75f};
    float px[5] = {x - half, x + half, x + half, x - half, x - half};
    float pz[5] = {z + half, z + half, z - half, z - half, z + half};
    for (int i = 0; i < 4; i++) {
        shade(c, k[i]);
        glVertex3f(px[i], 0, pz[i]);
        glVertex3f(px[i + 1], 0, pz[i + 1]);
        glVertex3f(x, h, z);
    }
    glEnd();
}

static void disc(float x, float y, float z, float r)
{
    glBegin(GL_TRIANGLE_FAN);
    glVertex3f(x, y, z);
    for (int i = 0; i <= 32; i++) {
        double a = 2 * PI * i / 32;
        glVertex3f(x + r * (float)cos(a), y + r * (float)sin(a), z);
    }
    glEnd();
}

/* The Haskell logo, ">λ=", 17 by 12 units, centred on the origin in the
 * x-y plane. Each piece is drawn as convex quads. */
static void haskell_logo(void)
{
    static const float dark[3] = {0x45 / 255.f, 0x3a / 255.f, 0x62 / 255.f};
    static const float mid[3] = {0x5e / 255.f, 0x50 / 255.f, 0x86 / 255.f};
    static const float light[3] = {0x8f / 255.f, 0x4e / 255.f, 0x8b / 255.f};
    /* Corners in the logo's usual SVG coordinates, y down. */
    static const float pieces[][8] = {
        {0, 0, 3, 0, 7, 6, 4, 6},        /* > upper */
        {4, 6, 7, 6, 3, 12, 0, 12},      /* > lower */
        {4, 0, 7, 0, 11, 6, 8, 6},       /* λ upper */
        {8, 6, 11, 6, 15, 12, 12, 12},   /* λ long stroke */
        {8, 6, 9.5f, 8.25f, 7, 12, 4, 12}, /* λ short leg */
        {10.33f, 3.5f, 17, 3.5f, 17, 5.5f, 11.66f, 5.5f}, /* = upper */
        {12.33f, 6.5f, 17, 6.5f, 17, 8.5f, 13.66f, 8.5f}, /* = lower */
    };
    const float *colour[] = {dark, dark, mid, mid, mid, light, light};
    glBegin(GL_QUADS);
    for (int i = 0; i < 7; i++) {
        glColor3fv(colour[i]);
        for (int v = 0; v < 4; v++)
            glVertex3f(pieces[i][2 * v] - 8.5f, 6 - pieces[i][2 * v + 1], 0);
    }
    glEnd();
}

static void text(int x, int y, const char *s)
{
    glRasterPos2i(x, y);
    glCallLists((GLsizei)strlen(s), GL_UNSIGNED_BYTE, s);
}

static void label3(float x, float y, float z, const char *s)
{
    glRasterPos3f(x, y, z);
    glCallLists((GLsizei)strlen(s), GL_UNSIGNED_BYTE, s);
}

static void perspective(float fovy_deg, float znear, float zfar)
{
    float aspect = (float)width / (float)(height > 0 ? height : 1);
    float top = znear * (float)tan(fovy_deg * PI / 360);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glFrustum(-top * aspect, top * aspect, -top, top, znear, zfar);
    glMatrixMode(GL_MODELVIEW);
}

/* The eye's transform within the seat. Games' signs: yaw + is left, pitch +
 * is down, roll + is tilted left, x + is left, y + is up, z + is back. */
static void head_view(void)
{
    glLoadIdentity();
    glRotatef(-pose.roll, 0, 0, 1);
    glRotatef(pose.pitch, 1, 0, 0);
    glRotatef(-pose.yaw, 0, 1, 0);
    glTranslatef(pose.x / 1000.f, -pose.y / 1000.f, -pose.z / 1000.f);
}

/* --- Scene -------------------------------------------------------------- */

static const float sky[3] = {0.53f, 0.71f, 0.92f};

static void build_world(void)
{
    world_list = glGenLists(1);
    glNewList(world_list, GL_COMPILE);

    /* Ground, runway and grid lie in one plane, so they're drawn in order
     * without the depth test. Small tiles keep the per-vertex fog right. */
    glDisable(GL_DEPTH_TEST);
    glBegin(GL_QUADS);
    for (int i = -30; i < 30; i++)
        for (int j = -30; j < 30; j++) {
            float g = ((i + j) & 1) ? 0.40f : 0.36f;
            glColor3f(0.20f, g, 0.18f);
            float x = i * 100.f, z = j * 100.f;
            quad(x, 0, z, x + 100, 0, z, x + 100, 0, z + 100, x, 0, z + 100);
        }
    glColor3f(0.35f, 0.35f, 0.37f);
    for (int j = -12; j < 1; j++)
        quad(-22, 0, j * 100.f, 22, 0, j * 100.f, 22, 0, j * 100.f + 100, -22, 0, j * 100.f + 100);
    glColor3f(0.95f, 0.95f, 0.95f);
    for (int j = -60; j < 2; j++)
        quad(-0.5f, 0, j * 20.f, 0.5f, 0, j * 20.f, 0.5f, 0, j * 20.f + 10, -0.5f, 0, j * 20.f + 10);
    glEnd();
    glColor3f(0.16f, 0.30f, 0.14f);
    glBegin(GL_LINES);
    for (int i = -30; i <= 30; i++)
        for (int j = -30; j < 30; j++) {
            float a = i * 100.f, b = j * 100.f;
            glVertex3f(a, 0, b);
            glVertex3f(a, 0, b + 100);
            glVertex3f(b, 0, a);
            glVertex3f(b + 100, 0, a);
        }
    glEnd();
    glEnable(GL_DEPTH_TEST);

    /* A landmark in each direction, so where you're looking is plain. */
    static const float red[3] = {0.85f, 0.15f, 0.12f};
    static const float white[3] = {0.95f, 0.95f, 0.95f};
    static const float yellow[3] = {0.95f, 0.80f, 0.15f};
    static const float blue[3] = {0.20f, 0.35f, 0.85f};
    static const float green[3] = {0.10f, 0.55f, 0.25f};
    static const float orange[3] = {0.95f, 0.50f, 0.10f};
    static const float purple[3] = {0.55f, 0.25f, 0.70f};
    static const float grey[3] = {0.60f, 0.60f, 0.62f};
    for (int i = 0; i < 6; i++) /* ahead: striped tower */
        box(-6, i * 12.f, -256, 6, i * 12.f + 12, -244, (i & 1) ? white : red);
    pyramid(250, 0, 40, 60, yellow); /* right */
    box(-50, 0, 260, 50, 35, 340, blue); /* behind: hangar */
    box(-256, 0, -6, -244, 90, 6, green); /* left: tall tower */
    box(140, 0, -160, 170, 30, -130, orange); /* ahead right */
    pyramid(-170, 170, 25, 45, purple);       /* behind left */
    box(170, 0, 165, 180, 55, 175, grey);    /* behind right */

    /* Ahead left: a Haskell logo billboard on two posts, facing the seat. */
    glPushMatrix();
    glTranslatef(-160, 0, -160);
    glRotatef(45, 0, 1, 0);
    box(-20, 0, -1.5f, -18, 25, 0.5f, grey);
    box(18, 0, -1.5f, 20, 25, 0.5f, grey);
    box(-32, 18, -1.5f, 32, 64, -0.5f, white);
    glTranslatef(0, 41, 0);
    glScalef(3, 3, 3);
    haskell_logo();
    glPopMatrix();

    glEndList();
}

static void draw_world(double t)
{
    glEnable(GL_FOG);
    glCallList(world_list);

    /* A Haskell logo spinning in the sky, ahead and to the right. */
    glPushMatrix();
    glTranslatef(120, 110, -300);
    glRotatef((float)fmod(t * 30, 360), 0, 1, 0);
    glScalef(4, 4, 4);
    haskell_logo();
    glPopMatrix();
    glDisable(GL_FOG);

    glDisable(GL_DEPTH_TEST);
    glColor3f(1, 1, 1);
    label3(0, 80, -250, "AHEAD");
    label3(250, 70, 0, "RIGHT");
    label3(0, 45, 300, "BEHIND");
    label3(-250, 100, 0, "LEFT");
    glEnable(GL_DEPTH_TEST);
}

/* The cockpit, in seat coordinates: the eye's rest position is the origin,
 * forward is -z. */
static void draw_cockpit(void)
{
    static const float body[3] = {0.45f, 0.47f, 0.50f};
    static const float panel[3] = {0.18f, 0.19f, 0.21f};
    static const float dial[3] = {0.05f, 0.05f, 0.06f};

    box(-0.45f, -0.70f, -0.76f, 0.45f, -0.20f, -0.70f, panel);  /* instrument panel */
    box(-0.48f, -0.20f, -0.86f, 0.48f, -0.16f, -0.62f, panel);  /* glare shield */
    box(-0.58f, -0.90f, -0.90f, -0.46f, -0.25f, 0.55f, body);   /* left sill */
    box(0.46f, -0.90f, -0.90f, 0.58f, -0.25f, 0.55f, body);     /* right sill */
    box(-0.22f, -0.55f, 0.28f, 0.22f, 0.12f, 0.38f, panel);     /* headrest */
    box(-0.58f, -1.00f, -2.6f, 0.58f, -0.25f, -0.86f, body);    /* nose */
    box(-6.0f, -0.95f, -0.3f, -0.58f, -0.85f, 1.2f, body);      /* left wing */
    box(0.58f, -0.95f, -0.3f, 6.0f, -0.85f, 1.2f, body);        /* right wing */

    /* Dials either side of a small Haskell logo. */
    static const float dx[4] = {-0.32f, -0.17f, 0.17f, 0.32f};
    for (int row = 0; row < 2; row++)
        for (int i = 0; i < 4; i++) {
            float x = dx[i], y = row ? -0.55f : -0.35f, z = -0.699f;
            shade(dial, 1);
            disc(x, y, z, 0.055f);
            glColor3f(0.95f, 0.95f, 0.95f);
            glBegin(GL_LINES);
            double a = 0.7 + i * 1.3 + row * 2.1;
            glVertex3f(x, y, z + 0.001f);
            glVertex3f(x + 0.045f * (float)cos(a), y + 0.045f * (float)sin(a), z + 0.001f);
            glEnd();
        }
    glPushMatrix();
    glTranslatef(0, -0.45f, -0.698f);
    glScalef(0.007f, 0.007f, 0.007f);
    haskell_logo();
    glPopMatrix();

    /* Canopy frame: a windscreen arch, a bow overhead and a spine. */
    static const float arch[][2] = {{-0.50f, -0.20f}, {-0.46f, 0.12f}, {-0.30f, 0.30f}, {0.f, 0.36f},
                                    {0.30f, 0.30f},   {0.46f, 0.12f},  {0.50f, -0.20f}};
    glLineWidth(6);
    glColor3f(0.12f, 0.12f, 0.14f);
    glBegin(GL_LINE_STRIP);
    for (int i = 0; i < 7; i++)
        glVertex3f(arch[i][0] * 0.95f, arch[i][1], -0.88f);
    glEnd();
    glBegin(GL_LINE_STRIP);
    for (int i = 0; i < 7; i++)
        glVertex3f(arch[i][0] * 1.1f, arch[i][1] * 1.2f + 0.05f, 0.15f);
    glEnd();
    glBegin(GL_LINES);
    glVertex3f(0, 0.36f, -0.88f);
    glVertex3f(0, 0.48f, 0.15f);
    glEnd();
    /* Boresight cross on the glare shield. */
    glLineWidth(2);
    glColor3f(0.2f, 1.0f, 0.3f);
    glBegin(GL_LINES);
    glVertex3f(-0.03f, -0.10f, -0.80f);
    glVertex3f(0.03f, -0.10f, -0.80f);
    glVertex3f(0, -0.13f, -0.80f);
    glVertex3f(0, -0.07f, -0.80f);
    glEnd();
    glLineWidth(1);
}

static void draw_overlay(float fps)
{
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(0, width, height, 0, -1, 1);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glDisable(GL_DEPTH_TEST);

    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glColor4f(0, 0, 0, 0.55f);
    glRecti(0, 0, width, 162);
    glDisable(GL_BLEND);

    char line[400];
    int y = 22;
    glColor3f(np.dll ? 0.85f : 1.0f, np.dll ? 0.85f : 0.4f, np.dll ? 0.85f : 0.4f);
    text(10, y, np.info);
    y += 20;
    if (!np.dll) {
        glColor3f(1, 0.4f, 0.4f);
        snprintf(line, sizeof line, "status: no NPClient, retrying every %d s", RETRY_MS / 1000);
    } else if (pose.ok) {
        glColor3f(0.4f, 1, 0.4f);
        snprintf(line, sizeof line, "status: ok   frame %d", pose.frame);
    } else {
        glColor3f(1, 0.75f, 0.3f);
        snprintf(line, sizeof line, "status: disabled (trackkat not running or not tracking)   frame %d", pose.frame);
    }
    text(10, y, line);
    y += 20;
    glColor3f(1, 1, 1);
    snprintf(line, sizeof line, "yaw %+7.1f   pitch %+7.1f   roll %+7.1f  deg", pose.yaw, pose.pitch, pose.roll);
    text(10, y, line);
    y += 20;
    snprintf(line, sizeof line, "x   %+7.1f   y     %+7.1f   z    %+7.1f  mm", pose.x, pose.y, pose.z);
    text(10, y, line);
    y += 20;
    snprintf(line, sizeof line, "checksum ok %u   bad %u", pose.cksum_ok, pose.cksum_bad);
    if (pose.cksum_bad)
        glColor3f(1, 0.4f, 0.4f);
    text(10, y, line);
    y += 20;
    glColor3f(1, 1, 1);
    snprintf(line, sizeof line, "%.0f fps", fps);
    text(10, y, line);
    y += 20;
    glColor3f(0.75f, 0.85f, 1);
    text(10, y, "C: recenter (NP_ReCenter)   F12: recenter (trackkat)   Esc: quit");

    const char *banner = NULL;
    if (!np.dll)
        banner = "NPClient not found";
    else if (!pose.ok)
        banner = "No tracking: start trackkat";
    else if (GetTickCount64() - recenter_shown_at < 1000)
        banner = "NP_ReCenter sent";
    if (banner) {
        int w = (int)strlen(banner) * 9;
        glEnable(GL_BLEND);
        glColor4f(0, 0, 0, 0.6f);
        glRecti(width / 2 - w / 2 - 16, height / 2 - 24, width / 2 + w / 2 + 16, height / 2 + 12);
        glDisable(GL_BLEND);
        glColor3f(1, 0.9f, 0.3f);
        text(width / 2 - w / 2, height / 2, banner);
    }
    glEnable(GL_DEPTH_TEST);
}

static void render(double t, float fps)
{
    glViewport(0, 0, width, height > 0 ? height : 1);
    glClearColor(sky[0], sky[1], sky[2], 1);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    perspective(70, 1.0f, 5000);
    head_view();
    glTranslatef(0, -EYE_HEIGHT, 0);
    draw_world(t);

    /* The cockpit is drawn over the world with its own near and far
     * planes, so close parts keep their depth precision. */
    glClear(GL_DEPTH_BUFFER_BIT);
    perspective(70, 0.02f, 20);
    head_view();
    draw_cockpit();

    draw_overlay(fps);
    SwapBuffers(hdc);
}

/* --- Window ------------------------------------------------------------- */

static LRESULT CALLBACK wndproc(HWND w, UINT msg, WPARAM wp, LPARAM lp)
{
    switch (msg) {
    case WM_SIZE:
        width = LOWORD(lp);
        height = HIWORD(lp);
        return 0;
    case WM_KEYDOWN:
        if (wp == VK_ESCAPE)
            DestroyWindow(w);
        else if (wp == 'C' && np.dll) {
            np.recenter();
            recenter_shown_at = GetTickCount64();
        }
        return 0;
    case WM_ERASEBKGND:
        return 1;
    case WM_DESTROY:
        PostQuitMessage(0);
        return 0;
    }
    return DefWindowProcW(w, msg, wp, lp);
}

static int setup_gl(void)
{
    PIXELFORMATDESCRIPTOR pfd;
    memset(&pfd, 0, sizeof pfd);
    pfd.nSize = sizeof pfd;
    pfd.nVersion = 1;
    pfd.dwFlags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER;
    pfd.iPixelType = PFD_TYPE_RGBA;
    pfd.cColorBits = 32;
    pfd.cDepthBits = 24;
    pfd.iLayerType = PFD_MAIN_PLANE;
    hdc = GetDC(hwnd);
    int format = ChoosePixelFormat(hdc, &pfd);
    if (!format || !SetPixelFormat(hdc, format, &pfd))
        return 0;
    HGLRC rc = wglCreateContext(hdc);
    if (!rc || !wglMakeCurrent(hdc, rc))
        return 0;

    swap_interval_fn swap_interval = (swap_interval_fn)(void *)wglGetProcAddress("wglSwapIntervalEXT");
    vsync = swap_interval && swap_interval(1);

    HFONT font = CreateFontW(-16, 0, 0, 0, FW_BOLD, FALSE, FALSE, FALSE, ANSI_CHARSET, OUT_DEFAULT_PRECIS,
                             CLIP_DEFAULT_PRECIS, ANTIALIASED_QUALITY, FIXED_PITCH | FF_MODERN, L"Consolas");
    SelectObject(hdc, font);
    font_base = glGenLists(96);
    if (!wglUseFontBitmapsW(hdc, 32, 96, font_base))
        wglUseFontBitmapsW(hdc, 32, 96, font_base);
    glListBase(font_base - 32);

    glEnable(GL_DEPTH_TEST);
    glFogi(GL_FOG_MODE, GL_LINEAR);
    glFogf(GL_FOG_START, 400);
    glFogf(GL_FOG_END, 3000);
    float fog[4] = {sky[0], sky[1], sky[2], 1};
    glFogfv(GL_FOG_COLOR, fog);
    glHint(GL_FOG_HINT, GL_NICEST);
    build_world();
    return 1;
}

int WINAPI WinMain(HINSTANCE inst, HINSTANCE prev, LPSTR cmd, int show)
{
    (void)prev;
    (void)cmd;
    WNDCLASSW wc;
    memset(&wc, 0, sizeof wc);
    wc.style = CS_OWNDC;
    wc.lpfnWndProc = wndproc;
    wc.hInstance = inst;
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    /* trackkat's icon (icons/trackkat.rc), or Windows' own if the build left it out. */
    wc.hIcon = LoadIcon(inst, MAKEINTRESOURCE(1));
    if (!wc.hIcon)
        wc.hIcon = LoadIcon(NULL, IDI_APPLICATION);
    wc.lpszClassName = L"trackdemo";
    RegisterClassW(&wc);

    RECT r = {0, 0, width, height};
    AdjustWindowRect(&r, WS_OVERLAPPEDWINDOW, FALSE);
    hwnd = CreateWindowW(L"trackdemo", L"trackkat demo", WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT,
                         r.right - r.left, r.bottom - r.top, NULL, NULL, inst, NULL);
    if (!hwnd || !setup_gl()) {
        MessageBoxW(NULL, L"Could not set up an OpenGL window.", L"trackdemo", MB_ICONERROR);
        return 1;
    }
    ShowWindow(hwnd, show);

    np_try_load();

    LARGE_INTEGER freq, start, now, fps_start;
    QueryPerformanceFrequency(&freq);
    QueryPerformanceCounter(&start);
    fps_start = start;
    int frames = 0;
    float fps = 0;
    MSG msg;
    for (;;) {
        while (PeekMessageW(&msg, NULL, 0, 0, PM_REMOVE)) {
            if (msg.message == WM_QUIT) {
                np_shutdown();
                return (int)msg.wParam;
            }
            TranslateMessage(&msg);
            DispatchMessageW(&msg);
        }
        np_poll();
        QueryPerformanceCounter(&now);
        double elapsed = (double)(now.QuadPart - fps_start.QuadPart) / (double)freq.QuadPart;
        frames++;
        if (elapsed >= 0.5) {
            fps = (float)(frames / elapsed);
            frames = 0;
            fps_start = now;
        }
        render((double)(now.QuadPart - start.QuadPart) / (double)freq.QuadPart, fps);
        if (IsIconic(hwnd))
            Sleep(50);
        else if (!vsync)
            Sleep(5);
    }
}
