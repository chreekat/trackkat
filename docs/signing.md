# Signing trackkat's builds

Windows SmartScreen warns about programs downloaded with a browser that
aren't signed by a publisher it knows. trackkat signs its own programs and
DLLs (not libusb's) in CI, before zipping them, so the SHA-256 that
installed copies check updates against is of the signed files. Updates that
trackkat downloads itself aren't marked as downloaded, so only the first
download ever meets SmartScreen; signing makes that one smoother too.

The **Sign** step of `.github/workflows/build.yml` runs on pushes to `main`
once the repository is set up for it, and is skipped until then. The build
uploads the unsigned files as the `trackkat-windows` artifact; SignPath
signs them and hands them back, and they replace the unsigned ones before
the zip, its SHA-256 and the release are made.

## SignPath Foundation

[SignPath Foundation](https://signpath.org/) signs open-source projects for
free, with a certificate in its own name ("SignPath Foundation"), after
checking that what it signs was built by CI from the project's public
repository.

1. **Apply** at <https://signpath.org/apply>. Its conditions, in short: an
   OSI-approved licence (trackkat's is MIT), a project that is maintained
   and has been released, no proprietary code of your own, and builds made
   by CI from the public repository. You must publish a code signing policy
   (a page or a README section naming who may commit, review and approve
   signing; SignPath gives a template) and use multi-factor authentication
   on GitHub and SignPath.
2. Once accepted, in SignPath (<https://app.signpath.io>), in the
   organization the Foundation sets up for the project:
   - Under **Trusted Build Systems**, link **GitHub.com** to the project.
     SignPath then checks each signing request's origin: that it comes from
     a GitHub-hosted runner (as `windows-latest`), building this repository,
     from the branch and workflow the policy allows (`main`,
     `.github/workflows/build.yml`).
   - In the project (its **slug**, say `trackkat`), add an **artifact
     configuration** from [`.signpath/artifact-configuration.xml`](../.signpath/artifact-configuration.xml),
     which says which files in the artifact to sign.
   - The Foundation provides the **signing policy** (`release-signing`):
     each request waits for an approver to approve it in SignPath, by web
     or e-mail. The step waits up to an hour.
   - Under your user's profile, or as a CI user, create an **API token**
     with permission to submit signing requests for the policy.
3. In the repository's **Settings → Secrets and variables → Actions**:

   | Variable | What |
   |---|---|
   | `SIGNPATH_ORGANIZATION_ID` | the organization's ID; signing is skipped while this is unset |
   | `SIGNPATH_PROJECT_SLUG` | the project's slug, as `trackkat` |
   | `SIGNPATH_SIGNING_POLICY_SLUG` | the signing policy's slug, as `release-signing` |
   | `SIGNPATH_ARTIFACT_CONFIGURATION_SLUG` | the artifact configuration's slug |

   | Secret | What |
   |---|---|
   | `SIGNPATH_API_TOKEN` | the API token |

## Other ways

- **A certificate in your own name**, from a certificate authority that
  signs in the cloud from CI, such as SSL.com's eSigner (an OV or EV
  certificate on their hardware, used through their `esigner-codesign`
  GitHub Action with your account's credentials and a TOTP secret as
  repository secrets). It costs a yearly fee, and the CA checks your
  identity. Replace the **Sign** step with theirs, signing the same files
  in `dist`.
- **Azure Artifact Signing** (formerly Trusted Signing) is cheaper, but
  signs for individuals only in the US and Canada.
