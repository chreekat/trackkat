/* fakepose: stand in for trackkat.exe, publishing a known moving pose, for
 * testing NPClient without a camera.
 *
 * usage: fakepose [seconds] */

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
    return 0;
}
