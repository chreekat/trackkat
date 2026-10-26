/* Shared memory between trackkat.exe and the NPClient DLL that games load.
 *
 * The FreeTrack heap layout follows OpenTrack's freetrackclient/fttypes.h
 * (ISC licence), itself from the FreeTrack project. */

#ifndef TRACKKAT_SHARED_H
#define TRACKKAT_SHARED_H

#include <stdint.h>

#define FREETRACK_HEAP "FT_SharedMem"
#define FREETRACK_MUTEX "FT_Mutext"
#define TRACKKAT_CONTROL "TrackKat_Control"
#define TRACKKAT_RUNNING_MUTEX "TrackKat_Running"

typedef struct FTData {
    uint32_t DataID; /* bumped on every new pose */
    int32_t CamWidth;
    int32_t CamHeight;
    float Yaw;   /* radians, positive to the left */
    float Pitch; /* radians, positive up */
    float Roll;  /* radians, positive to the left */
    float X;     /* mm */
    float Y;
    float Z;
    float RawYaw;
    float RawPitch;
    float RawRoll;
    float RawX;
    float RawY;
    float RawZ;
    float X1, Y1, X2, Y2, X3, Y3, X4, Y4;
} FTData;

typedef struct FTHeap {
    FTData data;
    int32_t GameID;
    unsigned char table[8];
    int32_t GameID2;
} FTHeap;

typedef struct TrackKatControl {
    volatile long recenter_requests; /* bumped by the DLL on NP_ReCenter */
    volatile long game_reads;        /* bumped by the DLL on NP_GetData */
} TrackKatControl;

#endif
