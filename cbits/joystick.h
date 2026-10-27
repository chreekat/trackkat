/* What the hotkey thread (windows.c) and the joystick code (joystick.c) ask
 * of each other. */

#ifndef TRACKKAT_JOYSTICK_H
#define TRACKKAT_JOYSTICK_H

#include <windows.h>

#define JOYSTICK_NAME_LEN 128 /* a joystick name and its terminator */

/* Actions, as bits of what hts_hotkeys_take returns. */
#define HOTKEY_RECENTER 1
#define HOTKEY_PAUSE 2
#define HOTKEY_TRIM 4

/* joystick.c: ask for every joystick's input, and to hear of joysticks
 * coming and going, through the calling thread's messages. Returns 0 or a
 * Windows error code. */
DWORD joysticks_watch(void);

/* windows.c: the actions bound to a joystick button, given the joystick's
 * name and which of those with that name it is, from 1. */
LONG hotkeys_for_button(const wchar_t *joystick, int index, int button);

/* windows.c: note actions pressed for, for hts_hotkeys_take. */
void hotkeys_press(LONG actions);

#endif
