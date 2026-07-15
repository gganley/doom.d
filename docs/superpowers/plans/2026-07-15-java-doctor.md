# java-doctor Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A `bin/java-doctor` zsh script (plus `doctor.el` shim for `doom doctor`) that detects drift between the JDK pins in config.el, jdtls's bundled Gradle Tooling API, and the installed toolchains — before it surfaces as broken editor imports.

**Architecture:** One self-contained zsh script holding five checks, configured entirely by env-var-overridable paths so tests run against throwaway fixtures. A checked-in test script (`bin/java-doctor-test`) builds fixtures and asserts on output lines and exit codes. A ~15-line `doctor.el` maps the script's `ERROR:`/`WARN:` lines to doom's `error!`/`warn!` during `doom doctor` (the `$DOOMDIR` is doom's `:user` module, whose `doctor.el` doom-doctor loads automatically).

**Tech Stack:** zsh, BSD (macOS) `find`/`date`, curl, emacs batch mode for the shim test.

**Spec:** `docs/superpowers/specs/2026-07-15-java-doctor-design.md`

## Global Constraints

- Output contract (verbatim from spec): one finding per line, prefixed `OK: `, `INFO: `, `WARN: `, or `ERROR: `. Exit 0 when no errors, exit 1 when at least one error. Warnings never affect the exit code.
- Env-var overrides (used by every test): `JAVA_DOCTOR_CONFIG_EL`, `JAVA_DOCTOR_JDTLS_PLUGINS`, `JAVA_DOCTOR_GRADLE_DAEMON_DIR`, `JAVA_DOCTOR_SDKMAN_JAVA_DIR`, plus `JAVA_DOCTOR_SKIP_NETWORK=1` and `JAVA_DOCTOR_LATEST_DATE=YYYYMMDD` for the freshness check.
- Checks skip gracefully with `INFO: skipped …` when inputs are absent. Unexpected conditions degrade to `WARN`, never a crash.
- macOS/BSD tooling only: `date -j -f %Y%m%d`, `find -mtime -7`. No GNU-isms.
- Tests must not touch the network (`JAVA_DOCTOR_SKIP_NETWORK=1` is set by the harness for every run).
- The repo working tree has unrelated uncommitted changes. Every commit must name exact paths (`git add <paths> && git commit -m "…" -- <paths>`); never `git add -A`.
- Working directory for all commands: `/Users/gregory.ganley/.config/doom`.

---

### Task 1: Script skeleton, test harness, and Check 1 (pin integrity)

**Files:**
- Create: `bin/java-doctor` (executable)
- Create: `bin/java-doctor-test` (executable)

**Interfaces:**
- Produces for later tasks: emit helpers `ok`/`info`/`warn`/`error` (each takes one string; `error` increments `errors`), globals `CONFIG_EL`, `PLUGINS_DIR`, `DAEMON_DIR`, `SDKMAN_JAVA_DIR`, `PIN_JDTLS_JAVA`, `PIN_IMPORT_HOME`, `IMPORT_MAJOR` (set by `check_pins`), helpers `java_major <java-binary>`, `path_major <path>`, `config_pin <elisp-var-name>`. Test harness functions `make_jdk <version>`, `run_doctor [KEY=VAL …]` (reads globals `cfg`/`plugins`/`daemon`/`sdkman`, sets `out`/`status`), `expect_line <pattern> <desc>`, `expect_no_line <pattern> <desc>`, `expect_exit <code> <desc>`.
- The run-checks section and the test-file summary footer are the anchors later tasks insert into; their exact text matters.

- [ ] **Step 1: Write the failing test harness**

Create `bin/java-doctor-test` with exactly:

```zsh
#!/bin/zsh
# Regression tests for bin/java-doctor. Builds throwaway fixtures and runs
# the doctor against them via its JAVA_DOCTOR_* env overrides.
set -u

DOCTOR=${0:a:h}/java-doctor
FIX=$(mktemp -d)
trap 'rm -rf "$FIX"' EXIT
fails=0 out= status=0
cfg= plugins= daemon= sdkman=

# Fake JDK: a version-named dir whose java shim prints a matching banner.
make_jdk() { # <version, e.g. 25.0.2> -> prints the JDK home path
  local dir=$FIX/jdks/$1-fake
  mkdir -p $dir/bin
  cat > $dir/bin/java <<EOF
#!/bin/sh
echo 'openjdk version "$1" 2026-01-01' >&2
EOF
  chmod +x $dir/bin/java
  print -r -- $dir
}

run_doctor() { # optional extra KEY=VAL env pairs as args
  out=$(env \
    JAVA_DOCTOR_CONFIG_EL=${cfg:-$FIX/absent-config.el} \
    JAVA_DOCTOR_JDTLS_PLUGINS=${plugins:-$FIX/absent-plugins} \
    JAVA_DOCTOR_GRADLE_DAEMON_DIR=${daemon:-$FIX/absent-daemon} \
    JAVA_DOCTOR_SDKMAN_JAVA_DIR=${sdkman:-$FIX/absent-sdkman} \
    JAVA_DOCTOR_SKIP_NETWORK=1 \
    "$@" "$DOCTOR" 2>&1)
  status=$?
}

expect_line() { # <grep pattern> <description>
  if print -r -- "$out" | grep -q -- "$1"; then
    print "PASS: $2"
  else
    print "FAIL: $2 — no line matching: $1"
    print -r -- "$out" | sed 's/^/    | /'
    fails=$(( fails + 1 ))
  fi
}

expect_no_line() { # <grep pattern> <description>
  if print -r -- "$out" | grep -q -- "$1"; then
    print "FAIL: $2 — unexpected line matching: $1"
    print -r -- "$out" | sed 's/^/    | /'
    fails=$(( fails + 1 ))
  else
    print "PASS: $2"
  fi
}

expect_exit() { # <code> <description>
  if [[ $status -eq $1 ]]; then
    print "PASS: $2"
  else
    print "FAIL: $2 — exit $status, wanted $1"
    fails=$(( fails + 1 ))
  fi
}

# --- check 1: pin integrity -------------------------------------------------
jdk25=$(make_jdk 25.0.2)
jdk21=$(make_jdk 21.0.9)

# Happy path: both pins exist and majors match their path names.
cfg=$FIX/good-config.el
cat > $cfg <<EOF
(after! lsp-java
  (setq lsp-java-java-path "$jdk25/bin/java"
        lsp-java-import-gradle-java-home "$jdk21"
        lsp-java-compile-null-analysis-mode "automatic"))
EOF
run_doctor
expect_line '^OK: lsp-java-java-path is JDK 25' "happy pins: jdtls JVM ok"
expect_line '^OK: Gradle-import JVM is JDK 21' "happy pins: import JVM ok"
expect_exit 0 "happy pins exit 0"

# Missing pin paths must error.
cfg=$FIX/bad-config.el
cat > $cfg <<'EOF'
(after! lsp-java
  (setq lsp-java-java-path "/nonexistent/jdk/bin/java"
        lsp-java-import-gradle-java-home "/nonexistent/jdk-home"))
EOF
run_doctor
expect_line '^ERROR: pinned lsp-java-java-path does not exist' "missing jdtls JVM pin errors"
expect_line '^ERROR: pinned lsp-java-import-gradle-java-home has no bin/java' "missing import home errors"
expect_exit 1 "missing pins exit 1"

# Path names JDK 21 but the binary reports 25: drifted pin must error.
mkdir -p $FIX/jdks/21.0.9-lies/bin
cp $jdk25/bin/java $FIX/jdks/21.0.9-lies/bin/java
cfg=$FIX/mismatch-config.el
cat > $cfg <<EOF
(setq lsp-java-java-path "$FIX/jdks/21.0.9-lies/bin/java")
EOF
run_doctor
expect_line '^ERROR: lsp-java-java-path names JDK 21 but reports major 25' "major mismatch errors"

# ---------------------------------------------------------------- summary --
print
if (( fails > 0 )); then
  print "$fails test(s) failed"
  exit 1
fi
print "all tests passed"
```

Then: `chmod +x bin/java-doctor-test`

- [ ] **Step 2: Run tests to verify they fail**

Run: `bin/java-doctor-test`
Expected: every test FAILs (the harness can't run `bin/java-doctor` because it doesn't exist yet — `run_doctor` output will show "no such file or directory"), summary line reports failures, exit 1.

- [ ] **Step 3: Write the script skeleton with Check 1**

Create `bin/java-doctor` with exactly:

```zsh
#!/bin/zsh
# java-doctor — detect drift between the JDK pins in config.el, jdtls's
# bundled Gradle Tooling API, and the JDKs installed on this machine, before
# it surfaces as broken editor imports.
# Spec: docs/superpowers/specs/2026-07-15-java-doctor-design.md
#
# Output contract: one finding per line, prefixed OK:/INFO:/WARN:/ERROR:.
# Exit 0 when no errors; exit 1 when at least one ERROR. Warnings never fail.

set -u

CONFIG_EL=${JAVA_DOCTOR_CONFIG_EL:-$HOME/.config/doom/config.el}
PLUGINS_DIR=${JAVA_DOCTOR_JDTLS_PLUGINS:-$HOME/.config/emacs/.local/etc/lsp/eclipse.jdt.ls/plugins}
DAEMON_DIR=${JAVA_DOCTOR_GRADLE_DAEMON_DIR:-$HOME/.gradle/daemon}
SDKMAN_JAVA_DIR=${JAVA_DOCTOR_SDKMAN_JAVA_DIR:-/opt/homebrew/opt/sdkman-cli/libexec/candidates/java}

errors=0
ok()    { print -r -- "OK: $1" }
info()  { print -r -- "INFO: $1" }
warn()  { print -r -- "WARN: $1" }
error() { print -r -- "ERROR: $1"; errors=$(( errors + 1 )) }

# Major version reported by a java binary (handles legacy "1.8.0" strings).
java_major() {
  local v major
  v=$("$1" -version 2>&1 | head -1 | sed -n 's/.*version "\([0-9._]*\)".*/\1/p')
  [[ -z $v ]] && return 1
  major=${v%%.*}
  if [[ $major == 1 ]]; then
    v=${v#1.}
    major=${v%%.*}
  fi
  print -r -- $major
}

# Major implied by a versioned path segment like .../25.0.2-tem/bin/java.
path_major() {
  print -r -- "$1" | grep -o '[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*' | head -1 | cut -d. -f1
}

# Quoted string value of an elisp variable pin in config.el.
config_pin() {
  grep -o "$1 \"[^\"]*\"" "$CONFIG_EL" 2>/dev/null | head -1 | sed 's/[^"]*"\(.*\)"/\1/'
}

# Check 1: pinned JDK paths exist and report the major their name claims.
# Sets PIN_JDTLS_JAVA / PIN_IMPORT_HOME / IMPORT_MAJOR for later checks.
PIN_JDTLS_JAVA= PIN_IMPORT_HOME= IMPORT_MAJOR=
check_pins() {
  if [[ ! -r $CONFIG_EL ]]; then
    warn "config.el not readable at $CONFIG_EL; skipping pin checks"
    return
  fi
  PIN_JDTLS_JAVA=$(config_pin lsp-java-java-path)
  PIN_IMPORT_HOME=$(config_pin lsp-java-import-gradle-java-home)

  if [[ -z $PIN_JDTLS_JAVA ]]; then
    warn "lsp-java-java-path is not pinned in $CONFIG_EL"
  elif [[ ! -x $PIN_JDTLS_JAVA ]]; then
    error "pinned lsp-java-java-path does not exist: $PIN_JDTLS_JAVA"
  else
    local want got
    want=$(path_major "$PIN_JDTLS_JAVA")
    got=$(java_major "$PIN_JDTLS_JAVA") || got=
    if [[ -z $got ]]; then
      warn "could not run pinned lsp-java-java-path: $PIN_JDTLS_JAVA"
    elif [[ -n $want && $want != $got ]]; then
      error "lsp-java-java-path names JDK $want but reports major $got: $PIN_JDTLS_JAVA"
    else
      ok "lsp-java-java-path is JDK $got ($PIN_JDTLS_JAVA)"
    fi
  fi

  if [[ -z $PIN_IMPORT_HOME ]]; then
    warn "lsp-java-import-gradle-java-home is not pinned in $CONFIG_EL"
  elif [[ ! -x $PIN_IMPORT_HOME/bin/java ]]; then
    error "pinned lsp-java-import-gradle-java-home has no bin/java: $PIN_IMPORT_HOME"
  else
    local want got
    want=$(path_major "$PIN_IMPORT_HOME")
    got=$(java_major "$PIN_IMPORT_HOME/bin/java") || got=
    if [[ -z $got ]]; then
      warn "could not run pinned import JVM: $PIN_IMPORT_HOME/bin/java"
    elif [[ -n $want && $want != $got ]]; then
      error "lsp-java-import-gradle-java-home names JDK $want but reports major $got: $PIN_IMPORT_HOME"
    else
      IMPORT_MAJOR=$got
      ok "Gradle-import JVM is JDK $got ($PIN_IMPORT_HOME)"
    fi
  fi
}

# -- run checks --------------------------------------------------------------
check_pins

(( errors > 0 )) && exit 1
exit 0
```

Then: `chmod +x bin/java-doctor`

- [ ] **Step 4: Run tests to verify they pass**

Run: `bin/java-doctor-test`
Expected: all 7 assertions PASS, "all tests passed", exit 0.

- [ ] **Step 5: Commit**

```bash
git add bin/java-doctor bin/java-doctor-test
git commit -m "Add java-doctor skeleton with pin-integrity check" -- bin/java-doctor bin/java-doctor-test
```

---

### Task 2: Check 2 — sdkman `current` drift notice

**Files:**
- Modify: `bin/java-doctor` (add one function; add one call line)
- Modify: `bin/java-doctor-test` (add tests before the summary footer)

**Interfaces:**
- Consumes: `info`, `PIN_JDTLS_JAVA`, `SDKMAN_JAVA_DIR` from Task 1.
- Produces: `check_sdkman_drift` (no args, emits only `INFO:` lines).

- [ ] **Step 1: Write the failing tests**

In `bin/java-doctor-test`, insert immediately BEFORE the `# ---------------------------------------------------------------- summary --` line:

```zsh
# --- check 2: sdkman drift (informational) ----------------------------------
sdkman=$FIX/sdkman
mkdir -p $sdkman
ln -s $FIX/jdks/25.0.2-fake $sdkman/current
cfg=$FIX/good-config.el
run_doctor
expect_line '^INFO: sdkman current (.*) matches the lsp-java-java-path pin' "sdkman match is INFO"
expect_exit 0 "sdkman match exit 0"

rm $sdkman/current
ln -s $FIX/jdks/21.0.9-fake $sdkman/current
run_doctor
expect_line '^INFO: sdkman current is .*21.0.9-fake; pins differ' "sdkman drift is INFO"
expect_no_line '^ERROR:.*sdkman' "sdkman drift never errors"
expect_exit 0 "sdkman drift exit 0"
sdkman=
```

- [ ] **Step 2: Run tests to verify the new ones fail**

Run: `bin/java-doctor-test`
Expected: Task 1 assertions PASS; the two `expect_line` sdkman assertions FAIL (no such output yet); exit 1.

- [ ] **Step 3: Implement the check**

In `bin/java-doctor`, insert immediately BEFORE the `# -- run checks -----` line:

```zsh
# Check 2: sdkman `current` symlink vs the pins — informational only,
# because the pins are intentional; this just makes drift visible.
check_sdkman_drift() {
  local cur=$SDKMAN_JAVA_DIR/current target tb
  if [[ ! -h $cur ]]; then
    info "skipped sdkman drift check: no symlink at $cur"
    return
  fi
  target=$(readlink "$cur")
  tb=${target:t}
  if [[ -n $PIN_JDTLS_JAVA && $PIN_JDTLS_JAVA == *"/$tb/"* ]]; then
    info "sdkman current ($tb) matches the lsp-java-java-path pin"
  else
    info "sdkman current is $tb; pins differ (intentional — lsp-java-java-path: ${PIN_JDTLS_JAVA:-unset})"
  fi
}
```

And change the run-checks section from:

```zsh
# -- run checks --------------------------------------------------------------
check_pins
```

to:

```zsh
# -- run checks --------------------------------------------------------------
check_pins
check_sdkman_drift
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `bin/java-doctor-test`
Expected: all assertions PASS, exit 0.

- [ ] **Step 5: Commit**

```bash
git add bin/java-doctor bin/java-doctor-test
git commit -m "java-doctor: add sdkman current-symlink drift notice" -- bin/java-doctor bin/java-doctor-test
```

---

### Task 3: Check 3 — Gradle Tooling API vs import JVM compatibility matrix

**Files:**
- Modify: `bin/java-doctor` (add two functions; add one call line)
- Modify: `bin/java-doctor-test` (add tests before the summary footer)

**Interfaces:**
- Consumes: `ok`/`info`/`error`, `PLUGINS_DIR`, `IMPORT_MAJOR` from Task 1.
- Produces: `gradle_max_java <gradle-version>` (prints max supported Java major, or empty string when the version is newer than the table), `check_toolingapi`.

- [ ] **Step 1: Write the failing tests**

In `bin/java-doctor-test`, insert immediately BEFORE the summary footer line:

```zsh
# --- check 3: toolingapi compatibility matrix --------------------------------
# jdtls bundles Gradle 8.9 (max Java 22); import JVM pinned to 25 must error.
plugins=$FIX/plugins-89
mkdir -p $plugins
touch $plugins/org.gradle.toolingapi_8.9.0.v20250531-0227-s.jar
cfg=$FIX/import25-config.el
cat > $cfg <<EOF
(setq lsp-java-java-path "$jdk25/bin/java"
      lsp-java-import-gradle-java-home "$jdk25")
EOF
run_doctor
expect_line '^ERROR: jdtls embedded Gradle 8.9.0 supports at most Java 22 but the Gradle-import JVM is Java 25' "8.9 vs JDK25 errors"
expect_exit 1 "8.9 vs JDK25 exit 1"

# Same tooling API with import JVM 21 is compatible.
cfg=$FIX/good-config.el
run_doctor
expect_line '^OK: jdtls embedded Gradle 8.9.0 (max Java 22) is compatible with import JVM (Java 21)' "8.9 vs JDK21 ok"
expect_exit 0 "8.9 vs JDK21 exit 0"

# Tooling API newer than the table is assumed OK.
plugins=$FIX/plugins-99
mkdir -p $plugins
touch $plugins/org.gradle.toolingapi_9.9.0.v20270101-0000-s.jar
cfg=$FIX/import25-config.el
run_doctor
expect_line '^OK: jdtls embedded Gradle Tooling API 9.9.0 is newer than the compat table' "newer-than-table assumed ok"
expect_exit 0 "newer-than-table exit 0"
plugins=
```

- [ ] **Step 2: Run tests to verify the new ones fail**

Run: `bin/java-doctor-test`
Expected: earlier assertions PASS; the three new `expect_line` assertions FAIL; exit 1.

- [ ] **Step 3: Implement the check**

In `bin/java-doctor`, insert immediately BEFORE the `# -- run checks -----` line:

```zsh
# Max Java a Gradle version can RUN on (per Gradle's compatibility docs),
# keyed as major*100+minor descending. Prints empty when the given version
# is newer than the table — callers treat that as "assume OK".
gradle_max_java() { # <gradle version, e.g. 8.9.0>
  local major=${1%%.*} rest=${1#*.} minor key i
  minor=${rest%%.*}
  key=$(( major * 100 + minor ))
  local -a table=( 901 25  814 24  810 23  808 22  805 21  803 20  706 19  705 18  703 17 )
  if (( key > table[1] )); then
    print -r -- ""
    return
  fi
  for (( i = 1; i <= $#table; i += 2 )); do
    if (( key >= table[i] )); then
      print -r -- ${table[i+1]}
      return
    fi
  done
  print -r -- 16
}

# Check 3: when a Gradle import root has no wrapper, Buildship falls back to
# the Gradle distribution matching jdtls's embedded Tooling API. That
# distribution must be able to run on the pinned import JVM, or wrapper-less
# imports die with "Unsupported class file major version" (2026-07-15 incident).
check_toolingapi() {
  if [[ ! -d $PLUGINS_DIR ]]; then
    info "skipped toolingapi check: jdtls not installed at $PLUGINS_DIR"
    return
  fi
  local jar gv max
  jar=$(print -l -- $PLUGINS_DIR/org.gradle.toolingapi_*.jar(N) | head -1)
  if [[ -z $jar ]]; then
    info "skipped toolingapi check: no org.gradle.toolingapi jar in $PLUGINS_DIR"
    return
  fi
  gv=$(basename "$jar" | sed 's/org\.gradle\.toolingapi_\([0-9.]*\)\.v.*/\1/')
  max=$(gradle_max_java "$gv")
  if [[ -z $max ]]; then
    ok "jdtls embedded Gradle Tooling API $gv is newer than the compat table; assuming modern JDK support"
  elif [[ -n $IMPORT_MAJOR ]] && (( IMPORT_MAJOR > max )); then
    error "jdtls embedded Gradle $gv supports at most Java $max but the Gradle-import JVM is Java $IMPORT_MAJOR — wrapper-less imports will fail with 'Unsupported class file major version'; pin lsp-java-import-gradle-java-home to a JDK <= $max or update jdtls"
  else
    ok "jdtls embedded Gradle $gv (max Java $max) is compatible with import JVM (Java ${IMPORT_MAJOR:-unknown})"
  fi
}
```

And change the run-checks section from:

```zsh
check_pins
check_sdkman_drift
```

to:

```zsh
check_pins
check_sdkman_drift
check_toolingapi
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `bin/java-doctor-test`
Expected: all assertions PASS, exit 0.

- [ ] **Step 5: Commit**

```bash
git add bin/java-doctor bin/java-doctor-test
git commit -m "java-doctor: add toolingapi vs import-JVM compatibility check" -- bin/java-doctor bin/java-doctor-test
```

---

### Task 4: Check 4 — Gradle daemon-log canary

**Files:**
- Modify: `bin/java-doctor` (add one function; add one call line)
- Modify: `bin/java-doctor-test` (add tests before the summary footer)

**Interfaces:**
- Consumes: `ok`/`info`/`error`, `DAEMON_DIR` from Task 1.
- Produces: `check_daemon_logs`.

- [ ] **Step 1: Write the failing tests**

In `bin/java-doctor-test`, insert immediately BEFORE the summary footer line:

```zsh
# --- check 4: daemon-log canary ----------------------------------------------
daemon=$FIX/daemon
mkdir -p $daemon/8.9
cat > $daemon/8.9/daemon-1.out.log <<'EOF'
BUG! exception in phase 'semantic analysis' in source unit '_BuildScript_' Unsupported class file major version 70
EOF
cfg=$FIX/good-config.el
run_doctor
expect_line '^ERROR: recent Gradle daemon log shows a JDK/Gradle incompatibility: .*daemon-1.out.log' "canary in daemon log errors"
expect_exit 1 "daemon canary exit 1"

rm $daemon/8.9/daemon-1.out.log
run_doctor
expect_line "^OK: no 'Unsupported class file major version' in Gradle daemon logs" "clean daemon logs ok"
expect_exit 0 "clean daemon logs exit 0"
daemon=
```

- [ ] **Step 2: Run tests to verify the new ones fail**

Run: `bin/java-doctor-test`
Expected: earlier assertions PASS; the two new `expect_line` assertions FAIL; exit 1.

- [ ] **Step 3: Implement the check**

In `bin/java-doctor`, insert immediately BEFORE the `# -- run checks -----` line:

```zsh
# Check 4: scan recent Gradle daemon logs for the classfile-version canary —
# the direct symptom of a too-new JVM running a too-old Gradle.
check_daemon_logs() {
  if [[ ! -d $DAEMON_DIR ]]; then
    info "skipped daemon-log check: no $DAEMON_DIR"
    return
  fi
  local hits f
  hits=$(find "$DAEMON_DIR" -name '*.log' -mtime -7 -exec grep -l 'Unsupported class file major version' {} + 2>/dev/null)
  if [[ -z $hits ]]; then
    ok "no 'Unsupported class file major version' in Gradle daemon logs from the last 7 days"
    return
  fi
  for f in ${(f)hits}; do
    error "recent Gradle daemon log shows a JDK/Gradle incompatibility: $f"
  done
}
```

And add `check_daemon_logs` to the run-checks section, after `check_toolingapi`:

```zsh
check_pins
check_sdkman_drift
check_toolingapi
check_daemon_logs
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `bin/java-doctor-test`
Expected: all assertions PASS, exit 0.

- [ ] **Step 5: Commit**

```bash
git add bin/java-doctor bin/java-doctor-test
git commit -m "java-doctor: add Gradle daemon-log canary check" -- bin/java-doctor bin/java-doctor-test
```

---

### Task 5: Check 5 — jdtls freshness

**Files:**
- Modify: `bin/java-doctor` (add one function; add one call line)
- Modify: `bin/java-doctor-test` (add tests before the summary footer)

**Interfaces:**
- Consumes: `ok`/`info`/`warn`, `PLUGINS_DIR` from Task 1.
- Produces: `check_freshness`. Honors `JAVA_DOCTOR_LATEST_DATE=YYYYMMDD` (test override, takes precedence) and `JAVA_DOCTOR_SKIP_NETWORK=1`.

- [ ] **Step 1: Write the failing tests**

In `bin/java-doctor-test`, insert immediately BEFORE the summary footer line:

```zsh
# --- check 5: jdtls freshness -------------------------------------------------
# Installed build stamp comes from plugin filenames (v20250531 fixture).
plugins=$FIX/plugins-89
cfg=$FIX/good-config.el
run_doctor JAVA_DOCTOR_LATEST_DATE=20260701
expect_line '^WARN: installed jdtls build 20250531 is [0-9]* days behind latest (20260701)' "stale jdtls warns"
expect_exit 0 "stale jdtls still exit 0 (warning only)"

run_doctor JAVA_DOCTOR_LATEST_DATE=20250610
expect_line '^OK: jdtls build 20250531 is [0-9]* days behind latest (20250610)' "fresh jdtls ok"
expect_exit 0 "fresh jdtls exit 0"

# Without a latest date and with network skipped, the check skips as INFO.
run_doctor
expect_line '^INFO: skipped freshness network check' "offline freshness skips"
plugins=
```

- [ ] **Step 2: Run tests to verify the new ones fail**

Run: `bin/java-doctor-test`
Expected: earlier assertions PASS; the three new `expect_line` assertions FAIL; exit 1.

- [ ] **Step 3: Implement the check**

In `bin/java-doctor`, insert immediately BEFORE the `# -- run checks -----` line:

```zsh
# Check 5: age of the installed jdtls vs the latest published build. Covers
# the root fragility — an aging jdtls means an aging embedded Tooling API.
check_freshness() {
  if [[ ! -d $PLUGINS_DIR ]]; then
    info "skipped freshness check: jdtls not installed at $PLUGINS_DIR"
    return
  fi
  local installed latest remote i_s l_s days
  installed=$(ls "$PLUGINS_DIR" | grep -o 'v20[0-9]\{6\}' | sort | tail -1 | tr -d v)
  if [[ -z $installed ]]; then
    info "skipped freshness check: no build stamps in plugin filenames"
    return
  fi
  latest=${JAVA_DOCTOR_LATEST_DATE:-}
  if [[ -z $latest ]]; then
    if [[ ${JAVA_DOCTOR_SKIP_NETWORK:-0} == 1 ]]; then
      info "skipped freshness network check (JAVA_DOCTOR_SKIP_NETWORK=1)"
      return
    fi
    remote=$(curl -fsS --max-time 3 https://download.eclipse.org/jdtls/snapshots/latest.txt 2>/dev/null)
    if [[ -z $remote ]]; then
      info "skipped freshness check: could not reach download.eclipse.org"
      return
    fi
    latest=$(print -r -- "$remote" | grep -o '20[0-9]\{10\}' | head -1 | cut -c1-8)
    if [[ -z $latest ]]; then
      info "skipped freshness check: could not parse latest.txt ($remote)"
      return
    fi
  fi
  i_s=$(date -j -f %Y%m%d "$installed" +%s 2>/dev/null)
  l_s=$(date -j -f %Y%m%d "$latest" +%s 2>/dev/null)
  if [[ -z $i_s || -z $l_s ]]; then
    warn "freshness check could not parse dates (installed=$installed latest=$latest)"
    return
  fi
  days=$(( (l_s - i_s) / 86400 ))
  if (( days > 90 )); then
    warn "installed jdtls build $installed is $days days behind latest ($latest) — bump it: set lsp-java-jdt-download-url to the newest release tarball, rm -rf ~/.config/emacs/.local/etc/lsp/eclipse.jdt.ls, restart Emacs, then M-x lsp-install-server RET jdtls RET"
  else
    ok "jdtls build $installed is $days days behind latest ($latest)"
  fi
}
```

And add `check_freshness` to the run-checks section, after `check_daemon_logs`:

```zsh
check_pins
check_sdkman_drift
check_toolingapi
check_daemon_logs
check_freshness
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `bin/java-doctor-test`
Expected: all assertions PASS, exit 0.

- [ ] **Step 5: Commit**

```bash
git add bin/java-doctor bin/java-doctor-test
git commit -m "java-doctor: add jdtls freshness check" -- bin/java-doctor bin/java-doctor-test
```

---

### Task 6: doctor.el shim and real-world verification

**Files:**
- Create: `doctor.el` (in the repo root, `$DOOMDIR/doctor.el`)

**Interfaces:**
- Consumes: `bin/java-doctor`'s output contract (Global Constraints) and the `doom-user-dir` variable plus `error!`/`warn!` macros that `bin/doom-doctor` defines when it loads module doctor.el files.
- Produces: nothing consumed later; terminal integration.

- [ ] **Step 1: Write the failing shim test**

The test runs emacs in batch mode with stub `error!`/`warn!` macros and `doom-user-dir` pointed at a fixture containing a fake `bin/java-doctor`, then asserts the mapping. Run these exact commands:

```bash
SHIMFIX=$(mktemp -d)
mkdir -p "$SHIMFIX/bin"
printf '#!/bin/sh\necho "OK: fine"\necho "WARN: stale thing"\necho "ERROR: broken thing"\nexit 1\n' > "$SHIMFIX/bin/java-doctor"
chmod +x "$SHIMFIX/bin/java-doctor"
emacs --batch --eval "(progn
  (defmacro error! (fmt &rest args) (list 'princ (append (list 'format (concat \"MAPPED-ERROR \" fmt \"\n\")) args)))
  (defmacro warn! (fmt &rest args) (list 'princ (append (list 'format (concat \"MAPPED-WARN \" fmt \"\n\")) args)))
  (defvar doom-user-dir \"$SHIMFIX/\")
  (load \"/Users/gregory.ganley/.config/doom/doctor.el\" nil t))"
```

Expected NOW: the `load` fails with "Cannot open load file" — doctor.el doesn't exist. (Keep `$SHIMFIX` for step 3.)

- [ ] **Step 2: Implement doctor.el**

Create `doctor.el` with exactly:

```elisp
;;; doctor.el -*- lexical-binding: t; -*-
;; Loaded by `doom doctor' ($DOOMDIR is doom's :user module, and doom-doctor
;; loads every module's doctor.el). Surfaces bin/java-doctor findings as
;; doctor warnings/errors; see that script for the checks themselves.

(let ((script (expand-file-name "bin/java-doctor" doom-user-dir)))
  (when (file-executable-p script)
    (condition-case err
        (dolist (line (split-string (shell-command-to-string
                                     (shell-quote-argument script))
                                    "\n" t))
          (cond ((string-prefix-p "ERROR: " line)
                 (error! "java-doctor: %s" (substring line 7)))
                ((string-prefix-p "WARN: " line)
                 (warn! "java-doctor: %s" (substring line 6)))))
      (error (warn! "java-doctor failed to run: %s" err)))))
```

- [ ] **Step 3: Re-run the shim test to verify it passes**

Re-run the `emacs --batch` command from step 1 (same `$SHIMFIX`).
Expected output, exactly these two lines:

```
MAPPED-WARN java-doctor: stale thing
MAPPED-ERROR java-doctor: broken thing
```

(`OK:` line silently ignored.) Then `rm -rf "$SHIMFIX"`.

- [ ] **Step 4: Clear the stale broken daemon logs**

The 2026-07-15 incident left canary strings in `~/.gradle/daemon/8.9/` logs from Gradle 8.9 daemons that are already dead (killed during the incident fix; Gradle 8.9 cannot run on any installed JDK, so nothing can use this directory). Without this, check 4 correctly ERRORs on week-old evidence of the already-fixed problem:

```bash
rm -rf ~/.gradle/daemon/8.9
```

- [ ] **Step 5: Real run of the doctor script**

Run: `bin/java-doctor; echo "exit: $?"`
Expected: `OK:` lines for both pins (JDK 25 / JDK 21), an `INFO:` sdkman drift notice (current → 25.0.2-tem, pin matches), `OK:` for the toolingapi check (embedded Gradle 8.9.0, max Java 22, import JVM 21), `OK:` for daemon logs, one `WARN:` from the freshness check (installed build 20250531 is >90 days behind — this is the expected nag until jdtls is bumped), and `exit: 0`.

- [ ] **Step 6: Real run of doom doctor**

Run: `doom doctor 2>&1 | tail -30`
Expected: the run completes; a warning containing `java-doctor: installed jdtls build 20250531 is` appears in the `:user` module section; no `java-doctor:` errors; the rest of the doctor output is undisturbed.

- [ ] **Step 7: Commit**

```bash
git add doctor.el
git commit -m "Add doctor.el shim: surface java-doctor findings in doom doctor" -- doctor.el
```
