/* npgame: load NPClient the way a TrackIR game does, and print what it
 * delivers. It unscrambles the data and checks the checksum as a game must,
 * so it shows whether a game would accept it.
 *
 * usage: npgame [game-id] [path\to\NPClient64.dll]
 * Without a path, it finds the DLL through the registry, like games do. */

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NP_AXIS_MAX 16383

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
typedef int(__stdcall *register_id_fn)(unsigned short);
typedef int(__stdcall *request_data_fn)(unsigned short);
typedef int(__stdcall *void_fn)(void);

/* The DCS key, as in npclient.c. */
static const unsigned char dcs_key[8] = {0x9B, 0xFC, 0x88, 0xF6, 0x8F, 0x86, 0x56, 0x05};

static unsigned cksum(unsigned char buf[], unsigned size)
{
    int rounds = size >> 2;
    int rem = size % 4;
    int c = size;
    int a0, a2;
    while (rounds != 0) {
        a0 = *(short int *)buf;
        a2 = *(short int *)(buf + 2);
        buf += 4;
        c += a0;
        a2 ^= (c << 5);
        a2 <<= 11;
        c ^= a2;
        c += (c >> 11);
        --rounds;
    }
    (void)rem; /* the record's size is a multiple of 4 */
    c ^= (c << 3);
    c += (c >> 5);
    c ^= (c << 4);
    c += (c >> 17);
    c ^= (c << 25);
    c += (c >> 6);
    return (unsigned)c;
}

static void unscramble(unsigned char buf[], unsigned size, const unsigned char table[8])
{
    unsigned table_ptr = 0;
    unsigned char var = 0x88;
    do {
        --size;
        unsigned char plain = buf[size] ^ table[table_ptr] ^ var;
        buf[size] = plain;
        var += size + plain;
        if (++table_ptr >= 8)
            table_ptr = 0;
    } while (size != 0);
}

static HMODULE load_from_registry(void)
{
    wchar_t dir[MAX_PATH];
    DWORD size = sizeof dir;
    if (RegGetValueW(HKEY_CURRENT_USER, L"Software\\NaturalPoint\\NATURALPOINT\\NPClient Location", L"Path",
                     RRF_RT_REG_SZ, NULL, dir, &size) != ERROR_SUCCESS) {
        fprintf(stderr, "no NPClient Location in the registry\n");
        return NULL;
    }
    wchar_t path[MAX_PATH];
    _snwprintf(path, MAX_PATH, L"%ls%ls", dir, sizeof(void *) == 8 ? L"NPClient64.dll" : L"NPClient.dll");
    printf("loading %ls\n", path);
    return LoadLibraryW(path);
}

int main(int argc, char **argv)
{
    unsigned short id = argc > 1 ? (unsigned short)atoi(argv[1]) : 1006;
    HMODULE dll = argc > 2 ? LoadLibraryA(argv[2]) : load_from_registry();
    if (!dll) {
        fprintf(stderr, "could not load NPClient (error %lu)\n", GetLastError());
        return 1;
    }
    get_data_fn get_data = (get_data_fn)(void *)GetProcAddress(dll, "NP_GetData");
    get_signature_fn get_signature = (get_signature_fn)(void *)GetProcAddress(dll, "NP_GetSignature");
    query_version_fn query_version = (query_version_fn)(void *)GetProcAddress(dll, "NP_QueryVersion");
    register_id_fn register_id = (register_id_fn)(void *)GetProcAddress(dll, "NP_RegisterProgramProfileID");
    request_data_fn request_data = (request_data_fn)(void *)GetProcAddress(dll, "NP_RequestData");
    void_fn start = (void_fn)(void *)GetProcAddress(dll, "NP_StartDataTransmission");
    void_fn recenter = (void_fn)(void *)GetProcAddress(dll, "NP_ReCenter");
    if (!get_data || !get_signature || !query_version || !register_id || !request_data || !start || !recenter) {
        fprintf(stderr, "NPClient is missing an export\n");
        return 1;
    }

    tir_signature_t sig;
    get_signature(&sig);
    printf("signature: %.40s...\n", sig.DllSignature);
    unsigned short version = 0;
    query_version(&version);
    printf("version: 0x%04x\n", version);
    register_id(id);
    request_data(0x77);
    start();
    recenter();

    int bad = 0;
    for (int i = 0; i < 40; i++) {
        tir_data_t d;
        int rc = get_data(&d);
        if (id == 1006)
            unscramble((unsigned char *)&d, sizeof d, dcs_key);
        unsigned want = d.cksum;
        d.cksum = 0;
        int ok = cksum((unsigned char *)&d, sizeof d) == want;
        bad += !ok;
        if (i % 4 == 0)
            printf("rc %d status %d frame %d checksum %s  yaw %+7.2f pitch %+7.2f roll %+7.2f deg  x %+6.1f y %+6.1f z %+6.1f mm\n",
                   rc, d.status, d.frame, ok ? "ok " : "BAD", d.yaw * 180.0 / NP_AXIS_MAX, d.pitch * 180.0 / NP_AXIS_MAX,
                   d.roll * 180.0 / NP_AXIS_MAX, d.tx * 500.0 / NP_AXIS_MAX, d.ty * 500.0 / NP_AXIS_MAX,
                   d.tz * 500.0 / NP_AXIS_MAX);
        Sleep(50);
    }
    printf("%d bad checksums\n", bad);
    return bad != 0;
}
