# java-doctor: Java editor-tooling drift detection

**Date:** 2026-07-15
**Status:** Approved

## Problem

On 2026-07-15, jdtls produced failing diagnostics across Java projects
("Unsupported class file major version 69/70", "missing required source
folder", "cannot be built until build path errors are resolved"). Root cause:
lsp-java's jdtls (May 2025 build) bundles Gradle Tooling API 8.9; when a
Gradle import root has no wrapper, Buildship falls back to its embedded
Gradle 8.9, which supports at most Java 22 — and every JDK installed on this
machine (sdkman 25, Homebrew 26) was too new. The failure was silent until it
surfaced as mystery diagnostics polluting editor sessions.

Two drift vectors caused it and will recur without detection:

1. JDKs advance (sdkman upgrades, `current` symlink moves) while pinned
   editor tooling stands still.
2. The jdtls install ages, so its bundled Gradle Tooling API falls behind
   the installed JDKs.

## Goal

Catch this class of drift at `doom doctor` time (or on demand), before it
surfaces as broken imports and noise diagnostics.

## Design

Two components, checked into this repo.

### Component 1: `bin/java-doctor` (executable zsh script)

**Configuration source.** The script greps `lsp-java-java-path` and
`lsp-java-import-gradle-java-home` values out of `config.el`. Pins are never
duplicated into the script.

**Output contract.** One finding per line, prefixed `OK:`, `INFO:`, `WARN:`,
or `ERROR:`. Exit 0 when no errors, exit 1 when at least one error. Warnings
do not affect the exit code. This is both the human interface and the
machine interface consumed by `doctor.el`.

**Checks.** Each check skips gracefully (`INFO: skipped <reason>`) when its
inputs are absent (jdtls not installed, no daemon logs, offline).

1. **Pin integrity** — both JDK paths extracted from config.el exist, and
   `java -version` reports the major version the pinned path name implies
   (e.g., a path containing `25.0.2-tem` must report major 25).
2. **Symlink drift** — compare the sdkman `current` symlink target against
   the pins. Informational only (`INFO`), since pins are intentional.
3. **Compatibility matrix** — extract the `org.gradle.toolingapi_X.Y.Z` jar
   version from the installed jdtls plugins directory; map Gradle version to
   its maximum supported Java via a small table in the script (versions newer
   than the table's max are assumed OK). `ERROR` if the pinned Gradle-import
   JVM's major exceeds that maximum. This check would have caught the
   2026-07-15 incident before any Java buffer was opened.
4. **Daemon-log canary** — grep Gradle daemon logs under `~/.gradle/daemon/`
   modified within the last 7 days for `Unsupported class file major
   version`. `ERROR` with the offending log path if found.
5. **jdtls freshness** — parse the installed jdtls build date from plugin
   filename stamps (e.g., `v20250531`); fetch the latest published version
   from Eclipse (curl, 3-second timeout, silent `INFO` skip when offline).
   `WARN` when the install is more than ~90 days behind, printing the bump
   procedure: set `lsp-java-jdt-download-url`, delete the server install
   dir, reinstall via `lsp-install-server`.

**Testability.** The config.el path, jdtls plugins directory, and Gradle
daemon directory are overridable via environment variables (defaulting to
the real locations), so each failure mode can be exercised against
throwaway fixtures without touching the real setup.

### Component 2: `doctor.el` (in `$DOOMDIR`)

Roughly 10 lines. `doom doctor` loads `$DOOMDIR/doctor.el` because the user
config dir is registered as the `:user` module (verified in doom-emacs
source: `doom-modules.el` registers `(:user . nil)` with `:path
doom-user-dir`, and `bin/doom-doctor` loads each module's `doctor.el`).

Behavior: if `bin/java-doctor` exists and is executable, run it; map
`ERROR:` lines to doom's `error!` and `WARN:` lines to `warn!`; pass
everything else silently. The call is wrapped so a broken script degrades to
a single warning instead of killing the doctor run.

## Error handling

The script never hard-fails the doctor run. Unexpected conditions inside a
check degrade to `WARN`. Missing optional inputs degrade to `INFO: skipped`.

## Testing

- Fixture runs for each failure mode via the env-var overrides: a config.el
  with a nonexistent pin path, a fake plugins dir with a stale
  `org.gradle.toolingapi_8.9.0` jar name and a too-new import JVM pin, a
  planted daemon log containing the canary string.
- One real run of `bin/java-doctor` (expected: quiet, exit 0 after the
  2026-07-15 fixes) and one real `doom doctor` run confirming the shim
  reports nothing and doesn't disturb the rest of the doctor output.

## Out of scope

- Scheduled/background execution (declined; `doom doctor` cadence is enough).
- Auto-updating jdtls (freshness check nags with instructions instead).
- Claude-session guardrails in Java repos' AGENTS.md (declined for now).
