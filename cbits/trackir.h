/* trackkat.exe's stand-in for NaturalPoint's TrackIR software: see
 * cbits/trackir.c. */

#ifndef TRACKIR_H
#define TRACKIR_H

#include <windows.h>

#include "trackkat_shared.h"

/* Our control block, which cbits/windows.c maps. */
volatile TrackKatControl *hts_control_block(void);

/* Hand the pose just published in FreeTrack shared memory to games using
 * NaturalPoint's NPClient, if the stand-in is running. Never waits long. */
void trackir_publish(const volatile FTData *heap);

/* Games' processes, as cbits/windows.c tells them apart. */
int hts_process_started(unsigned long pid, unsigned long long *created);
int hts_still_the_game(DWORD pid, unsigned long long *created, unsigned long long seen);
unsigned long long hts_now(void);

#endif
