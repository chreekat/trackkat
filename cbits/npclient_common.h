/* What our NPClient DLL and trackkat.exe, publishing (cbits/windows.c) and
 * standing in for NaturalPoint's software (cbits/trackir.c), need alike, so
 * they can't drift apart: TrackIR's pose record, how a pose is scaled into
 * it, checked and scrambled, NaturalPoint's signature, the games' keys, the
 * registry of games tracking in our control block, and the security of the
 * objects games share with trackkat. */

#ifndef NPCLIENT_COMMON_H
#define NPCLIENT_COMMON_H

#include <windows.h>

#include "trackkat_shared.h"

enum { NP_STATUS_OK = 0, NP_STATUS_DISABLED = 1 };

/* A pose older than this means trackkat has stopped publishing. */
#define NP_STALE_MS 1000

/* The record games read with NP_GetData. */
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

/* Tracks whether trackkat is still publishing, by when the FreeTrack pose's
 * DataID last changed. Starts zeroed. */
typedef struct np_freshness {
    unsigned data_id;
    unsigned long long changed_ms;
} np_freshness;

/* Whether the pose with this DataID, seen at now_ms (GetTickCount64), is
 * fresh. */
int np_pose_fresh(np_freshness *f, unsigned data_id, unsigned long long now_ms);

/* Fill in a record's status and axes from a FreeTrack pose: OK and the pose
 * when fresh, otherwise disabled and zeros. Leaves the frame alone. */
void np_fill_record(tir_data_t *d, const volatile FTData *pose, int fresh);

/* Checksum a filled-in record, then scramble it with a game's key, if it
 * has one. */
void np_seal_record(tir_data_t *d, const unsigned char *key);

/* Undo np_seal_record with the same key: whether the record's checksum
 * then checks out. */
int np_unseal_record(tir_data_t *d, const unsigned char *key);

/* The key a game's data is scrambled with, and the game's name, or NULL for
 * games that take plain data. */
const unsigned char *np_game_key(unsigned short id, const char **name);

/* NaturalPoint's signature, which games check. */
void np_signature(tir_signature_t *sig);

/* Security for the objects games share with trackkat, made with
 * np_share_with_games and freed with np_shared_security_free. */
typedef struct np_shared_security {
    SECURITY_ATTRIBUTES attributes;
    SECURITY_DESCRIPTOR descriptor;
    PSECURITY_DESCRIPTOR label; /* holds the descriptor's integrity label */
} np_shared_security;

/* The security attributes to create an object games share with trackkat
 * with, which live as long as *s. */
SECURITY_ATTRIBUTES *np_share_with_games(np_shared_security *s);
void np_shared_security_free(np_shared_security *s);

/* Record a process among the games tracking; 0 if there's no free slot. */
int np_add_game(volatile TrackKatControl *control, long pid);
void np_remove_game(volatile TrackKatControl *control, long pid);

#endif
