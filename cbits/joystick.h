/* What the hotkey thread (windows.c) and the joystick code (joystick.c) ask
 * of each other. */

#ifndef TRACKKAT_JOYSTICK_H
#define TRACKKAT_JOYSTICK_H

#include <windows.h>

#define JOYSTICK_NAME_LEN 128 /* a joystick name and its terminator */

/* joystick.c: ask for every joystick's input, and to hear of joysticks
 * coming and going, through the calling thread's messages. Returns 0 or a
 * Windows error code. */
DWORD joysticks_watch(void);

/* windows.c: note a joystick button going down, or coming up, if it is
 * watched, given the joystick's name and which of those with that name it
 * is, from 1. */
void watched_button(const wchar_t *joystick, int index, int button, int down);

#endif
