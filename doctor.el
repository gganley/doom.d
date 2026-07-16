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
