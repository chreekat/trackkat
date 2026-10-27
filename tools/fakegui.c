/* fakegui: drive trackkat's window and tray icon (cbits/gui.c) with a
 * scripted pose and status, for working on the GUI without a camera. Prints
 * what the user asks for. Keys chosen for the bindings are all Ctrl+F7.
 *
 * usage: fakegui [seconds] */

#include <windows.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

int hts_gui_start(int show);
void hts_gui_stop(void);
void hts_gui_log(const wchar_t *line);
void hts_gui_frame(int blobs, float yaw, float pitch, float roll, float x, float y, float z, float vyaw,
                   float vpitch, float vroll, float vx, float vy, float vz);
void hts_gui_camera(int camera, const wchar_t *text);
void hts_gui_paused(int paused);
void hts_gui_games(int games);
void hts_gui_stats(const wchar_t *text);
void hts_gui_calibration(const wchar_t *text);
void hts_gui_settings_message(const wchar_t *text);
void hts_gui_set_setting(const wchar_t *name, const wchar_t *value);
int hts_gui_saved_setting(const wchar_t *name, wchar_t *buf, int size);
int hts_gui_take_actions(void);
int hts_gui_chosen_key(int which);
int hts_gui_window_open(void);
int hts_gui_capturing(void);
void hts_gui_add_binding(int which, const wchar_t *binding);

static const wchar_t *const names[] = {L"recenter", L"pause", L"smoothing", L"steadiness", L"responsiveness",
                                       L"yaw", L"pitch", L"roll", L"x", L"y", L"z", L"lean", L"drift"};
static const wchar_t *const values[] = {L"F12, Throttle - HOTAS Warthog button 23", L"Ctrl+F9", L"on", L"90", L"45", L"1:6",
                                        L"1:7", L"1:4", L"1:3", L"1:4", L"1:2.5", L"view", L"on"};

int main(int argc, char **argv)
{
    int seconds = argc > 1 ? atoi(argv[1]) : 30;
    if (hts_gui_start(1) != 0) {
        fprintf(stderr, "could not start the GUI\n");
        return 1;
    }
    for (int i = 0; i < 13; i++)
        hts_gui_set_setting(names[i], values[i]);
    hts_gui_log(L"Games will load NPClient from C:\\Games\\trackkat");
    hts_gui_log(L"Recenter: F12   Pause: Ctrl+F9   (Enter also recenters, c Enter calibrates level)");
    hts_gui_log(L"Tracking.");
    hts_gui_calibration(L"Level learnt: camera tipped 12.4 degrees down and 0.8 to its right, from 1460 degrees of head "
                        L"turning.\nNeck pivot learnt: 92 mm below the clip, 71 behind it and 3 to its left, from 214 head "
                        L"turns.\nCentre has followed you since recentering: yaw +0.4  pitch -0.2  roll +0.0   x +21  y -2  z +6 mm");
    hts_gui_camera(1, L"");
    hts_gui_games(1);
    int paused = 0;
    for (int i = 0; i < seconds * 60; i++) {
        double t = i / 60.0;
        float yaw = (float)(14 * sin(t * 0.9)), pitch = (float)(-6 + 5 * sin(t * 1.3));
        float roll = (float)(3 * sin(t * 0.7)), x = (float)(10 * sin(t * 0.5));
        float y = (float)(-4 * sin(t * 0.8)), z = (float)(15 * sin(t * 0.4));
        int lost = fmod(t, 12) > 10;
        hts_gui_frame(lost ? 2 : -1, yaw, pitch, roll, x, y, z, yaw * 6, pitch * 7, roll * 4, x * 3, y * 4, z * 2.5f);
        if (i % 60 == 0)
            hts_gui_stats(L"latency 0.11 ms (max 0.42)  gap max 9 ms  stripes max 31  GC 3 (max 0.21 ms)");
        if (i % 600 == 300)
            hts_gui_log(i % 1200 == 300 ? L"Recentered." : L"Joystick button pressed: Throttle - HOTAS Warthog button 23");
        int acts = hts_gui_take_actions();
        if (acts) {
            printf("actions 0x%x, window %s\n", acts, hts_gui_window_open() ? "open" : "closed");
            fflush(stdout);
        }
        if (acts & 0x2) {
            paused = !paused;
            hts_gui_paused(paused);
        }
        if (acts & 0x10) {
            for (int f = 0; f < 13; f++) {
                wchar_t v[256];
                hts_gui_saved_setting(names[f], v, 256);
                printf("  %ls = %ls\n", names[f], v);
            }
            hts_gui_settings_message(L"Saved.");
        }
        /* Any key chosen stands for Ctrl+F7, as Haskell would name it. */
        if (acts & 0x60) {
            printf("  key %x\n", hts_gui_chosen_key(acts & 0x40 ? 1 : 0));
            hts_gui_add_binding(acts & 0x40 ? 1 : 0, L"Ctrl+F7");
        }
        if (acts & 0x8)
            break;
        Sleep(16);
    }
    hts_gui_stop();
    return 0;
}
