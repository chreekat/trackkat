/* fakenpclient: stands in for NaturalPoint's own NPClient64.dll, which
 * isn't in this repository, to test trackkat's stand-in for NaturalPoint's
 * software (cbits/trackir.c) without it. It does what cbits/trackir.c says
 * NaturalPoint's DLL does: it sends the software commands as message 0x405
 * to the window titled "NaturalPoint", and hands games the pose record from
 * Local\SharedTrackIRData, copied under NPClientMutex, as is. Like the real
 * one, it sends each command once, when the game calls for it, and holds
 * the shared memory open while loaded.
 *
 * Built as NPClient64.dll for npgame to load: see tools/test-npclient.sh. */

#include <windows.h>
#include <string.h>

#include "../cbits/npclient_common.h"

#define NP_EXPORT(t) __declspec(dllexport) t __stdcall

/* Where the software keeps the version, signature and pose record. */
#define SHARED_SIZE 0x1E6
#define SHARED_VERSION 0
#define SHARED_SIGNATURE 0x12
#define SHARED_DATA 0x1A2

static HANDLE mapping, mutex;
static volatile unsigned char *shared;

static int map_shared(void)
{
    if (shared)
        return 1;
    if (!mutex)
        mutex = CreateMutexA(NULL, FALSE, "NPClientMutex");
    if (!mapping)
        mapping = CreateFileMappingA(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0, SHARED_SIZE,
                                     "Local\\SharedTrackIRData");
    if (mapping)
        shared = MapViewOfFile(mapping, FILE_MAP_WRITE, 0, 0, SHARED_SIZE);
    return mutex && shared;
}

/* Send the software a command; 1 if it is running. */
static int command(WPARAM code, LPARAM arg)
{
    HWND software = FindWindowW(NULL, L"NaturalPoint");
    if (!software)
        return 0;
    SendMessageW(software, 0x405, code, arg);
    return 1;
}

static void read_shared(void *out, size_t offset, size_t size)
{
    if (!map_shared()) {
        memset(out, 0, size);
        return;
    }
    WaitForSingleObject(mutex, 100);
    memcpy(out, (const void *)(shared + offset), size);
    ReleaseMutex(mutex);
}

NP_EXPORT(int) NP_GetData(tir_data_t *data)
{
    read_shared(data, SHARED_DATA, sizeof *data);
    return 0;
}

NP_EXPORT(int) NP_GetSignature(tir_signature_t *sig)
{
    read_shared(sig, SHARED_SIGNATURE, sizeof *sig);
    return 0;
}

NP_EXPORT(int) NP_QueryVersion(unsigned short *version)
{
    read_shared(version, SHARED_VERSION, sizeof *version);
    return 0;
}

NP_EXPORT(int) NP_RegisterProgramProfileID(unsigned short id)
{
    map_shared();
    return !command(1010, id);
}

NP_EXPORT(int) NP_RegisterWindowHandle(HWND hwnd) { return !command(1020, (LPARAM)hwnd); }
NP_EXPORT(int) NP_UnregisterWindowHandle(void) { return !command(1021, 0); }
NP_EXPORT(int) NP_StartDataTransmission(void) { return !command(2010, 0); }
NP_EXPORT(int) NP_StopDataTransmission(void) { return !command(2020, 0); }
NP_EXPORT(int) NP_ReCenter(void) { return !command(3030, 0); }
NP_EXPORT(int) NP_RequestData(unsigned short req) { (void)req; return 0; }
