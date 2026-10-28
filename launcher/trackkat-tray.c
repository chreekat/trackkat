/* trackkat-tray.exe: start trackkat.exe, from this program's own folder, in
 * the system tray, with no console window. Windows runs it at login when
 * trackkat starts with Windows (startup = on): trackkat.exe is a console
 * program, which run directly would open a console window. */

#include <windows.h>
#include <string.h>
#include <wchar.h>

int WINAPI wWinMain(HINSTANCE instance, HINSTANCE previous, PWSTR args, int show)
{
    (void)instance;
    (void)previous;
    (void)args;
    (void)show;
    wchar_t path[MAX_PATH];
    DWORD n = GetModuleFileNameW(NULL, path, MAX_PATH);
    wchar_t *slash = n && n < MAX_PATH ? wcsrchr(path, L'\\') : NULL;
    if (!slash || (size_t)(slash + 1 - path) + wcslen(L"trackkat.exe") >= MAX_PATH)
        return 1;
    *slash = 0;
    wchar_t dir[MAX_PATH];
    wcscpy(dir, path);
    wcscpy(slash, L"\\trackkat.exe");

    wchar_t command[MAX_PATH + 16];
    _snwprintf(command, MAX_PATH + 16, L"\"%ls\" --tray", path);
    command[MAX_PATH + 15] = 0;
    STARTUPINFOW si;
    PROCESS_INFORMATION pi;
    memset(&si, 0, sizeof si);
    si.cb = sizeof si;
    if (!CreateProcessW(path, command, NULL, NULL, FALSE, CREATE_NO_WINDOW, NULL, dir, &si, &pi))
        return 1;
    CloseHandle(pi.hThread);
    CloseHandle(pi.hProcess);
    return 0;
}
