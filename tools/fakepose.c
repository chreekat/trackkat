/* fakepose: stand in for trackkat.exe, publishing a known moving pose, for
 * testing NPClient, ours or NaturalPoint's, without a camera.
 *
 * usage: fakepose [seconds [game]]
 *
 * At the end it prints the game NaturalPoint's NPClient was served, as
 * "served game: " and the five numbers trackkat saves; given them as game,
 * a later fakepose serves that game again, as a restarted trackkat does. */

#include <windows.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

int hts_shared_open(void);
void hts_publish(float yaw, float pitch, float roll, float x, float y, float z);
long hts_recenter_count(void);
long hts_game_reads(void);
int hts_single_instance(void);
int hts_games_tracking(void);
int hts_trackir_start(unsigned long pid, unsigned long long created, unsigned long long window, int profile,
                      int tracking);
long hts_trackir_served(unsigned long *pid, unsigned long long *created, unsigned long long *window, int *profile,
                        int *tracking);

int main(int argc, char **argv)
{
    int seconds = argc > 1 ? atoi(argv[1]) : 5;
    if (!hts_single_instance()) {
        fprintf(stderr, "trackkat (or another fakepose) is running\n");
        return 1;
    }
    if (hts_shared_open() != 0) {
        fprintf(stderr, "could not create shared memory\n");
        return 1;
    }
    unsigned long pid = 0;
    unsigned long long created = 0, window = 0;
    int profile = -1, tracking = 0;
    if (argc > 2 && sscanf(argv[2], "%lu %llu %llu %d %d", &pid, &created, &window, &profile, &tracking) != 5) {
        fprintf(stderr, "game: pid created window profile tracking\n");
        return 1;
    }
    int rc = hts_trackir_start(pid, created, window, profile, tracking);
    if (rc != 0) {
        fprintf(stderr, "could not stand in for NaturalPoint's software (%d)\n", rc);
        return 1;
    }
    const double deg = 3.14159265358979323846 / 180;
    int most_games = 0;
    for (int i = 0; i < seconds * 100; i++) {
        int games = hts_games_tracking();
        if (games > most_games)
            most_games = games;
        double t = i / 100.0;
        /* Yaw sweeps +-30 degrees; the rest are fixed, recognisable values. */
        hts_publish((float)(30 * sin(t) * deg), (float)(10 * deg), (float)(-5 * deg), 12, -34, 56);
        Sleep(10);
    }
    printf("games read %ld poses and asked to recenter %ld times\n", hts_game_reads(), hts_recenter_count());
    printf("at most %d games tracking, %d at the end\n", most_games, hts_games_tracking());
    hts_trackir_served(&pid, &created, &window, &profile, &tracking);
    printf("served game: %lu %llu %llu %d %d\n", pid, created, window, profile, tracking);
    return 0;
}
