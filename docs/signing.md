# Signing trackkat's builds

Windows SmartScreen warns about programs downloaded with a browser that
aren't signed by a publisher it knows. trackkat signs its own programs and
DLLs (not libusb's) in CI, before zipping them, so the SHA-256 that
installed copies check updates against is of the signed files. Updates that
trackkat downloads itself aren't marked as downloaded, so only the first
download ever meets SmartScreen; signing makes that one smooth too.

The **Sign** step of `.github/workflows/build.yml` runs on pushes to `main`
once the repository is set up for it, and is skipped until then.

## Azure Artifact Signing

[Artifact Signing](https://learn.microsoft.com/azure/artifact-signing/)
(formerly Trusted Signing) signs with a certificate Microsoft holds and
renews for you. The step uses
[`azure/artifact-signing-action`](https://github.com/Azure/artifact-signing-action).

In Azure:

1. Create an Artifact Signing account, and in it an identity validation and
   a certificate profile (public trust) for the publisher's name.
2. Register an app in Microsoft Entra ID for GitHub to sign in as, and give
   it the **Artifact Signing Certificate Profile Signer** role on the
   account (or just on the certificate profile).
3. Let GitHub sign in as the app, either:
   - with OIDC (no secret to keep): in the app's **Certificates & secrets →
     Federated credentials**, add one for GitHub Actions, organization
     `chreekat`, repository `trackkat`, entity **Branch**, branch `main`; or
   - with a client secret: create one under **Certificates & secrets**.

In the repository's **Settings → Secrets and variables → Actions**:

| Variable | What |
|---|---|
| `AZURE_SIGNING_ACCOUNT` | the Artifact Signing account's name; signing is skipped while this is unset |
| `AZURE_SIGNING_PROFILE` | the certificate profile's name |
| `AZURE_SIGNING_ENDPOINT` | the account's region's endpoint, as `https://eus.codesigning.azure.net/` |
| `AZURE_TENANT_ID` | the directory (tenant) ID |
| `AZURE_CLIENT_ID` | the app's (client) ID |
| `AZURE_SUBSCRIPTION_ID` | the subscription holding the account (OIDC only) |

| Secret | What |
|---|---|
| `AZURE_CLIENT_SECRET` | the app's client secret; leave it unset to sign in with OIDC |

## SignPath Foundation instead

[SignPath Foundation](https://signpath.org/) signs open-source projects for
free, with its own certificate. To use it, replace the **Sign in to Azure**
and **Sign** steps with SignPath's
[`signpath/github-action-submit-signing-request`](https://github.com/signpath/github-action-submit-signing-request):
upload `dist` as an artifact, submit it for signing, and download the
signed files back into `dist` before the **Publish dev release** step zips
them. SignPath's signing policy then names which files to sign. Keep the
step's condition, so builds sign only once it is set up.
