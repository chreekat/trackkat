/* Installing, updating and uninstalling trackkat.exe, per user, without
 * admin: known folders, the Start-menu shortcut, the uninstall entry and
 * Run key, starting and waiting for processes, downloading and hashing.
 * src/TrackKat/Install.hs decides what to do with them. */

#include <windows.h>
#include <bcrypt.h>
#include <objbase.h>
#include <shlobj.h>
#include <shobjidl.h>
#include <stdio.h>
#include <string.h>
#include <wchar.h>
#include <winhttp.h>

#define UNINSTALL_KEY L"Software\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\trackkat"
#define RUN_KEY L"Software\\Microsoft\\Windows\\CurrentVersion\\Run"
#define RUN_ONCE_KEY L"Software\\Microsoft\\Windows\\CurrentVersion\\RunOnce"
#define NPCLIENT_KEY L"Software\\NaturalPoint\\NATURALPOINT\\NPClient Location"

/* ---- Folders --------------------------------------------------------------- */

/* A known folder, as hts_known_folder numbers them: %LOCALAPPDATA%,
 * %APPDATA%, and the Start menu's Programs. */
static const GUID local_app_data = {0xF1B32785, 0x6FBA, 0x4FCF, {0x9D, 0x55, 0x7B, 0x8E, 0x7F, 0x15, 0x70, 0x91}};
static const GUID roaming_app_data = {0x3EB685DB, 0x65F9, 0x4CF6, {0xA0, 0x3A, 0xE3, 0xEF, 0x65, 0x72, 0x9F, 0x3D}};
static const GUID programs = {0xA77F5D77, 0x2E2B, 0x44C3, {0xA6, 0xA2, 0xAB, 0xA6, 0x01, 0x05, 0x4A, 0x51}};

/* Put a known folder (0 %LOCALAPPDATA%, 1 %APPDATA%, 2 the Start menu's
 * Programs) in a buffer of len characters. Returns 0, or an HRESULT. */
int hts_known_folder(int which, wchar_t *out, int len)
{
    const GUID *id = which == 0 ? &local_app_data : which == 1 ? &roaming_app_data : &programs;
    wchar_t *path = NULL;
    HRESULT hr = SHGetKnownFolderPath(id, 0, NULL, &path);
    if (FAILED(hr))
        return (int)hr;
    int fits = wcslen(path) < (size_t)len;
    if (fits)
        wcscpy(out, path);
    CoTaskMemFree(path);
    return fits ? 0 : (int)HRESULT_FROM_WIN32(ERROR_INSUFFICIENT_BUFFER);
}

/* ---- The Start-menu shortcut ------------------------------------------------ */

static const GUID clsid_shell_link = {0x00021401, 0, 0, {0xC0, 0, 0, 0, 0, 0, 0, 0x46}};
static const GUID iid_shell_link = {0x000214F9, 0, 0, {0xC0, 0, 0, 0, 0, 0, 0, 0x46}};
static const GUID iid_persist_file = {0x0000010B, 0, 0, {0xC0, 0, 0, 0, 0, 0, 0, 0x46}};

/* Write a shortcut to target, started in its own folder, with its icon.
 * Returns 0, or an HRESULT. */
int hts_make_shortcut(const wchar_t *link, const wchar_t *target, const wchar_t *description)
{
    HRESULT init = CoInitializeEx(NULL, COINIT_APARTMENTTHREADED);
    IShellLinkW *sl = NULL;
    IPersistFile *pf = NULL;
    HRESULT hr = CoCreateInstance(&clsid_shell_link, NULL, CLSCTX_INPROC_SERVER, &iid_shell_link, (void **)&sl);
    if (SUCCEEDED(hr)) {
        wchar_t dir[MAX_PATH];
        lstrcpynW(dir, target, MAX_PATH);
        wchar_t *slash = wcsrchr(dir, L'\\');
        if (slash)
            *slash = 0;
        sl->lpVtbl->SetPath(sl, target);
        sl->lpVtbl->SetWorkingDirectory(sl, dir);
        sl->lpVtbl->SetDescription(sl, description);
        sl->lpVtbl->SetIconLocation(sl, target, 0);
        hr = sl->lpVtbl->QueryInterface(sl, &iid_persist_file, (void **)&pf);
    }
    if (SUCCEEDED(hr))
        hr = pf->lpVtbl->Save(pf, link, TRUE);
    if (pf)
        pf->lpVtbl->Release(pf);
    if (sl)
        sl->lpVtbl->Release(sl);
    if (SUCCEEDED(init))
        CoUninitialize();
    return SUCCEEDED(hr) ? 0 : (int)hr;
}

/* ---- The registry ------------------------------------------------------------ */

static LONG set_string(HKEY key, const wchar_t *name, const wchar_t *value)
{
    return RegSetValueExW(key, name, 0, REG_SZ, (const BYTE *)value, (DWORD)((wcslen(value) + 1) * sizeof(wchar_t)));
}

static LONG set_dword(HKEY key, const wchar_t *name, DWORD value)
{
    return RegSetValueExW(key, name, 0, REG_DWORD, (const BYTE *)&value, sizeof value);
}

/* List trackkat in Settings' Apps, for the current user, with exe uninstalling
 * it. size_kb is the installed size. Returns 0, or a Windows error code. */
int hts_set_uninstall_entry(const wchar_t *exe, const wchar_t *location, const wchar_t *version, int size_kb)
{
    HKEY key;
    LONG rc = RegCreateKeyExW(HKEY_CURRENT_USER, UNINSTALL_KEY, 0, NULL, 0, KEY_SET_VALUE, NULL, &key, NULL);
    if (rc != ERROR_SUCCESS)
        return (int)rc;
    wchar_t command[MAX_PATH + 64];
    _snwprintf(command, MAX_PATH + 64, L"%ls,0", exe);
    command[MAX_PATH + 63] = 0;
    rc = set_string(key, L"DisplayIcon", command);
    if (rc == ERROR_SUCCESS)
        rc = set_string(key, L"DisplayName", L"trackkat");
    if (rc == ERROR_SUCCESS)
        rc = set_string(key, L"DisplayVersion", version);
    if (rc == ERROR_SUCCESS)
        rc = set_string(key, L"Publisher", L"chreekat");
    if (rc == ERROR_SUCCESS)
        rc = set_string(key, L"URLInfoAbout", L"https://github.com/chreekat/trackkat");
    if (rc == ERROR_SUCCESS)
        rc = set_string(key, L"InstallLocation", location);
    _snwprintf(command, MAX_PATH + 64, L"\"%ls\" --uninstall", exe);
    command[MAX_PATH + 63] = 0;
    if (rc == ERROR_SUCCESS)
        rc = set_string(key, L"UninstallString", command);
    _snwprintf(command, MAX_PATH + 64, L"\"%ls\" --uninstall --quiet", exe);
    command[MAX_PATH + 63] = 0;
    if (rc == ERROR_SUCCESS)
        rc = set_string(key, L"QuietUninstallString", command);
    if (rc == ERROR_SUCCESS)
        rc = set_dword(key, L"NoModify", 1);
    if (rc == ERROR_SUCCESS)
        rc = set_dword(key, L"NoRepair", 1);
    if (rc == ERROR_SUCCESS)
        rc = set_dword(key, L"EstimatedSize", (DWORD)size_kb);
    RegCloseKey(key);
    return (int)rc;
}

/* Returns 0, or a Windows error code; a missing entry is no error. */
int hts_remove_uninstall_entry(void)
{
    LONG rc = RegDeleteTreeW(HKEY_CURRENT_USER, UNINSTALL_KEY);
    return rc == ERROR_FILE_NOT_FOUND ? 0 : (int)rc;
}

/* Run command when the user logs in (once, if once is nonzero), as the
 * value named name. Returns 0, or a Windows error code. */
int hts_set_run(const wchar_t *name, const wchar_t *command, int once)
{
    HKEY key;
    LONG rc = RegCreateKeyExW(HKEY_CURRENT_USER, once ? RUN_ONCE_KEY : RUN_KEY, 0, NULL, 0, KEY_SET_VALUE, NULL, &key,
                              NULL);
    if (rc != ERROR_SUCCESS)
        return (int)rc;
    rc = set_string(key, name, command);
    RegCloseKey(key);
    return (int)rc;
}

/* Stop running the value named name at login. Returns 0, or a Windows error
 * code; a missing value is no error. */
int hts_remove_run(const wchar_t *name)
{
    HKEY key;
    LONG rc = RegOpenKeyExW(HKEY_CURRENT_USER, RUN_KEY, 0, KEY_SET_VALUE, &key);
    if (rc == ERROR_FILE_NOT_FOUND)
        return 0;
    if (rc != ERROR_SUCCESS)
        return (int)rc;
    rc = RegDeleteValueW(key, name);
    RegCloseKey(key);
    return rc == ERROR_FILE_NOT_FOUND ? 0 : (int)rc;
}

/* The folder games load NPClient from, in a buffer of len characters.
 * Returns 0 if games are pointed nowhere. */
int hts_get_npclient_path(wchar_t *out, int len)
{
    DWORD size = (DWORD)len * sizeof(wchar_t);
    return RegGetValueW(HKEY_CURRENT_USER, NPCLIENT_KEY, L"Path", RRF_RT_REG_SZ, NULL, out, &size) == ERROR_SUCCESS;
}

/* Point games nowhere. Returns 0, or a Windows error code. */
int hts_remove_npclient_path(void)
{
    HKEY key;
    LONG rc = RegOpenKeyExW(HKEY_CURRENT_USER, NPCLIENT_KEY, 0, KEY_SET_VALUE, &key);
    if (rc == ERROR_FILE_NOT_FOUND)
        return 0;
    if (rc != ERROR_SUCCESS)
        return (int)rc;
    rc = RegDeleteValueW(key, L"Path");
    RegCloseKey(key);
    return rc == ERROR_FILE_NOT_FOUND ? 0 : (int)rc;
}

/* ---- Processes ------------------------------------------------------------- */

/* Start a program with a command line (which names the program first), in
 * the given folder. A program given no window gets no console window; else
 * a console program shares this one's console, if it has one. Returns the
 * process ID, or minus a Windows error code. */
int hts_start(const wchar_t *program, wchar_t *command, const wchar_t *dir, int no_window)
{
    STARTUPINFOW si;
    PROCESS_INFORMATION pi;
    memset(&si, 0, sizeof si);
    si.cb = sizeof si;
    if (!CreateProcessW(program, command, NULL, NULL, FALSE, no_window ? CREATE_NO_WINDOW : 0, NULL, dir, &si, &pi))
        return -(int)GetLastError();
    CloseHandle(pi.hThread);
    CloseHandle(pi.hProcess);
    return (int)pi.dwProcessId;
}

/* Wait up to ms milliseconds for a process to end. Returns nonzero once it
 * has, or if there is no such process. */
int hts_wait_exit(int pid, int ms)
{
    HANDLE p = OpenProcess(SYNCHRONIZE, FALSE, (DWORD)pid);
    if (!p)
        return GetLastError() == ERROR_INVALID_PARAMETER;
    DWORD r = WaitForSingleObject(p, (DWORD)ms);
    CloseHandle(p);
    return r == WAIT_OBJECT_0;
}

int hts_own_pid(void)
{
    return (int)GetCurrentProcessId();
}

/* Ask a question in a message box, of a kind: 0 an OK, 1 yes, no or cancel.
 * Returns IDOK, IDYES, IDNO or IDCANCEL. */
int hts_ask(const wchar_t *text, int kind)
{
    UINT type = kind == 1 ? MB_YESNOCANCEL | MB_ICONQUESTION : MB_OK | MB_ICONINFORMATION;
    return MessageBoxW(NULL, text, L"trackkat", type | MB_SETFOREGROUND);
}

/* ---- Downloading and hashing ------------------------------------------------- */

/* Download an HTTPS URL to a file, following redirects, through the
 * system's proxy. Returns 0; a Windows error code; or, if the server
 * refused, minus its HTTP status. */
int hts_download(const wchar_t *url, const wchar_t *path)
{
    URL_COMPONENTS uc;
    wchar_t host[256], rest[2048];
    memset(&uc, 0, sizeof uc);
    uc.dwStructSize = sizeof uc;
    uc.lpszHostName = host;
    uc.dwHostNameLength = 256;
    uc.lpszUrlPath = rest;
    uc.dwUrlPathLength = 2048;
    if (!WinHttpCrackUrl(url, 0, 0, &uc))
        return (int)GetLastError();
    if (uc.nScheme != INTERNET_SCHEME_HTTPS)
        return ERROR_INVALID_PARAMETER;

    /* The automatic proxy, which follows the system's settings, is Windows
     * 8.1's; before, the default proxy. */
    HINTERNET session = WinHttpOpen(L"trackkat", 4 /* WINHTTP_ACCESS_TYPE_AUTOMATIC_PROXY */, WINHTTP_NO_PROXY_NAME,
                                    WINHTTP_NO_PROXY_BYPASS, 0);
    if (!session)
        session = WinHttpOpen(L"trackkat", WINHTTP_ACCESS_TYPE_DEFAULT_PROXY, WINHTTP_NO_PROXY_NAME,
                              WINHTTP_NO_PROXY_BYPASS, 0);
    if (!session)
        return (int)GetLastError();
    WinHttpSetTimeouts(session, 30000, 30000, 30000, 60000);
    int rc = 0;
    HINTERNET connection = WinHttpConnect(session, host, uc.nPort, 0);
    HINTERNET request = connection ? WinHttpOpenRequest(connection, L"GET", rest, NULL, WINHTTP_NO_REFERER,
                                                        WINHTTP_DEFAULT_ACCEPT_TYPES, WINHTTP_FLAG_SECURE)
                                   : NULL;
    if (!request || !WinHttpSendRequest(request, WINHTTP_NO_ADDITIONAL_HEADERS, 0, WINHTTP_NO_REQUEST_DATA, 0, 0, 0)
        || !WinHttpReceiveResponse(request, NULL))
        rc = (int)GetLastError();
    if (rc == 0) {
        DWORD status = 0, size = sizeof status;
        WinHttpQueryHeaders(request, WINHTTP_QUERY_STATUS_CODE | WINHTTP_QUERY_FLAG_NUMBER, WINHTTP_HEADER_NAME_BY_INDEX,
                            &status, &size, WINHTTP_NO_HEADER_INDEX);
        if (status != 200)
            rc = -(int)status;
    }
    HANDLE file = INVALID_HANDLE_VALUE;
    if (rc == 0) {
        file = CreateFileW(path, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
        if (file == INVALID_HANDLE_VALUE)
            rc = (int)GetLastError();
    }
    while (rc == 0) {
        static char buf[65536];
        DWORD got = 0, written;
        if (!WinHttpReadData(request, buf, sizeof buf, &got))
            rc = (int)GetLastError();
        else if (got == 0)
            break;
        else if (!WriteFile(file, buf, got, &written, NULL) || written != got)
            rc = (int)GetLastError();
    }
    if (file != INVALID_HANDLE_VALUE)
        CloseHandle(file);
    if (rc != 0)
        DeleteFileW(path);
    if (request)
        WinHttpCloseHandle(request);
    if (connection)
        WinHttpCloseHandle(connection);
    WinHttpCloseHandle(session);
    return rc;
}

/* The SHA-256 of a file, as 64 hex digits and a 0, in out. Returns 0, or a
 * Windows error code or NTSTATUS. */
int hts_sha256(const wchar_t *path, char *out)
{
    HANDLE file = CreateFileW(path, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    if (file == INVALID_HANDLE_VALUE)
        return (int)GetLastError();
    BCRYPT_ALG_HANDLE alg = NULL;
    BCRYPT_HASH_HANDLE hash = NULL;
    NTSTATUS st = BCryptOpenAlgorithmProvider(&alg, BCRYPT_SHA256_ALGORITHM, NULL, 0);
    if (st >= 0)
        st = BCryptCreateHash(alg, &hash, NULL, 0, NULL, 0, 0);
    int rc = st < 0 ? (int)st : 0;
    while (rc == 0) {
        static unsigned char buf[65536];
        DWORD got;
        if (!ReadFile(file, buf, sizeof buf, &got, NULL))
            rc = (int)GetLastError();
        else if (got == 0)
            break;
        else if ((st = BCryptHashData(hash, buf, got, 0)) < 0)
            rc = (int)st;
    }
    unsigned char digest[32];
    if (rc == 0 && (st = BCryptFinishHash(hash, digest, sizeof digest, 0)) < 0)
        rc = (int)st;
    if (rc == 0)
        for (int i = 0; i < 32; i++)
            sprintf(out + 2 * i, "%02x", digest[i]);
    if (hash)
        BCryptDestroyHash(hash);
    if (alg)
        BCryptCloseAlgorithmProvider(alg, 0);
    CloseHandle(file);
    return rc;
}
