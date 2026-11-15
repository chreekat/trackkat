/* fakekeys: drive trackkat's watching of keys and buttons (cbits/windows.c)
 * with made-up key and mouse events, and print the edges it tells of, for
 * working on it without pressing keys. Each line printed says what the
 * events before it should give. Build it with cbits/windows.c and
 * cbits/joystick.c, and -lhid.
 *
 * usage: fakekeys */

#include <windows.h>
#include <stdio.h>

#include "trackkat_shared.h"

void hts_bindings_clear(void);
void hts_bind_key(unsigned mods, unsigned vk);
void hts_bind_button(const wchar_t *joystick, int index, int button);
void hts_bindings_apply(void);
int hts_hotkeys_start(int *mouse_err, int *joysticks_err);
int hts_key_edge(int *key, wchar_t *joystick, int joystick_len, int *index, int *button, double *age);
void watched_button(const wchar_t *joystick, int index, int button, int down);

/* Stand-ins for cbits/trackir.c. */
void trackir_publish(volatile FTData *d) { (void)d; }

static void key(WORD vk, int up)
{
    INPUT in = {0};
    in.type = INPUT_KEYBOARD;
    in.ki.wVk = vk;
    in.ki.dwFlags = up ? KEYEVENTF_KEYUP : 0;
    SendInput(1, &in, sizeof in);
    Sleep(30);
}

static void side(int up)
{
    INPUT in = {0};
    in.type = INPUT_MOUSE;
    in.mi.dwFlags = up ? MOUSEEVENTF_XUP : MOUSEEVENTF_XDOWN;
    in.mi.mouseData = XBUTTON1;
    SendInput(1, &in, sizeof in);
    Sleep(30);
}

static void drain(const char *what)
{
    int k, index, button, edge;
    wchar_t name[128];
    double age;
    printf("%s:\n", what);
    while ((edge = hts_key_edge(&k, name, 128, &index, &button, &age)) != 0)
        printf("  %s key %#x joystick '%ls' (%d) button %d, %.0f ms ago\n", edge == 1 ? "down" : "up  ", k, name, index,
               button, age * 1000);
    fflush(stdout);
}

int main(void)
{
    hts_bindings_clear();
    hts_bind_key(0, VK_F8);
    hts_bind_key(0x2, VK_F9); /* Ctrl+F9 */
    hts_bind_key(0, VK_XBUTTON1);
    hts_bind_button(L"throttle - hotas warthog", 1, 4);
    hts_bind_button(L"Throttle - HOTAS Warthog", 1, 4);
    hts_bindings_apply();
    int mouse_err = 0, joystick_err = 0;
    int keyboard_err = hts_hotkeys_start(&mouse_err, &joystick_err);
    printf("errors: keyboard %d mouse %d joysticks %d\n", keyboard_err, mouse_err, joystick_err);

    key(VK_F8, 0);
    key(VK_F8, 0); /* a repeat */
    key(VK_F8, 0);
    Sleep(200);
    key(VK_F8, 1);
    drain("F8 held, repeating, and let go");

    key(VK_F7, 0);
    key(VK_F7, 1);
    key(VK_F9, 0);
    key(VK_F9, 1);
    drain("F7, and F9 with no Ctrl: neither watched");

    key(VK_CONTROL, 0);
    key(VK_F9, 0);
    key(VK_CONTROL, 1);
    key(VK_F9, 1);
    drain("Ctrl+F9, Ctrl let go first");

    side(0);
    side(1);
    drain("Mouse4");

    key(VK_F8, 0);
    hts_bindings_clear();
    hts_bind_key(0, VK_F7);
    hts_bindings_apply();
    key(VK_F8, 0); /* a repeat, of a key no longer watched */
    key(VK_F8, 1);
    key(VK_F8, 0);
    key(VK_F8, 1);
    drain("F8 held as it stops being watched");

    hts_bindings_clear();
    hts_bind_button(L"throttle - hotas warthog", 1, 4);
    hts_bind_button(L"Throttle - HOTAS Warthog", 1, 4);
    hts_bind_button(L"Throttle - HOTAS Warthog", 2, 4);
    hts_bindings_apply();
    watched_button(L"Throttle - HOTAS Warthog", 1, 4, 1);
    watched_button(L"Throttle - HOTAS Warthog", 1, 5, 1);
    watched_button(L"Throttle - HOTAS Warthog", 1, 4, 0);
    drain("a joystick's button 4, bound under two spellings, and 5, not watched");

    hts_bindings_clear();
    hts_bind_key(0, VK_F8);
    hts_bindings_apply();
    key(VK_F8, 0);
    Sleep(2100);
    key(VK_F8, 0); /* long after its last key-down: a new press */
    key(VK_F8, 1);
    drain("F8 down, never seen up, and down again two seconds on");
    return 0;
}
