;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Gregory Ganley"
      user-mail-address "gregory.ganley@savant.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-outrun-electric
      doom-font "Fira Code-13")
(require 'org-indent)
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "/Users/gregory.ganley/Documents/org-roam"
      org-roam-directory "/Users/gregory.ganley/Documents/org-roam"
      org-refile-allow-creating-parent-nodes 'confirm
      org-default-notes-file (expand-file-name "notes.org" org-directory)
      +org-capture-notes-file (expand-file-name "notes.org" org-directory)
      +org-capture-journal-file (expand-file-name "journal.org" org-directory))
;; (setq +format-on-save-disabled-modes
;;       '(emacs-lisp-mode  ; elisp's mechanisms are good enough
;;         sql-mode         ; sqlformat is currently broken
;;         tex-mode         ; latexindent is broken
;;         latex-mode))
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(setq forge-database-connector 'sqlite-builtin
      org-roam-database-connector 'sqlite-builtin
      org-agenda-file-menu-enabled nil)
(after! evil-escape (evil-escape-mode -1))
(setq frame-title-format
      '(""
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format "%s - " project-name))))
        (:eval (file-relative-name (if (not buffer-file-name) "vterm" (buffer-file-name) ) projectile-project-root))))
(setq org-link-file-path-type 'relative)
(setq doom-modeline-buffer-file-name-style 'relative-to-project)
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;;###autodef
(defun gg/hostname-split (str)
  (interactive)
  (let ((name (string-trim-right (nth 1 (split-string str "//")) "/")))
    (if (eq 1 (length (split-string name "/")))
        (format "%s/%s" name name)
      (replace-regexp-in-string "[?=#]" "" (string-trim-right name ".html")))))


(setq org-agenda-files (directory-files-recursively "~/Documents/org-roam" "\.org$")
      org-roam-dailies-directory "daily/"
      org-startup-folded 'showeverything
      org-capture-templates
      '(("t" "Personal todo" entry
         (file+headline +org-capture-todo-file "Inbox")
         "* TODO %?\n%i\n%a" :prepend t)
        ("n" "Personal notes" entry
         (file+headline +org-capture-notes-file "Inbox")
         "* %u %?\n%i\n%a" :prepend t)
        ("c" "Clocked item" entry
         (clock)
         "* %u %?\n%i\n%a" :prepend t)
        ("j" "Journal" entry
         (file+olp+datetree +org-capture-journal-file)
         "* %U %?\n%i\n%a" :prepend t)
        ("p" "Templates for projects")
        ("pt" "Project-local todo" entry
         (file+headline +org-capture-project-todo-file "Inbox")
         "* TODO %?\n%i\n%a" :prepend t)
        ("pn" "Project-local notes" entry
         (file+headline +org-capture-project-notes-file "Inbox")
         "* %U %?\n%i\n%a" :prepend t)
        ("pc" "Project-local changelog" entry
         (file+headline +org-capture-project-changelog-file "Unreleased")
         "* %U %?\n%i\n%a" :prepend t)
        ("o" "Centralized templates for projects")
        ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
        ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
        ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t)))
(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :target (file+head "capture/${slug}.org" "#+title: ${title}\n")
           :unnarrowed t))
        org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+datetree "%<%Y>.org" day)))
        org-roam-capture-ref-templates
        '(("r" "ref" plain
           "\n\n${body}\n\n%?"
           :target (file+head "sites/%(gg/hostname-split \"${ref}\").org" "#+title: ${title}\n#+roam_key: ${ref}\n")
           :immediate-finish t
           :empty-lines-before 1))
        org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))) ;; this last one fixes the node find issue with the new date structure
(setq org-agenda-prefix-format
      '((agenda . " %i")
        (todo . " %i")
        (tags . " %i")
        (search . " %i"))
      org-agenda-todo-ignore-scheduled 'future
      org-agenda-tags-todo-honor-ignore-options t)

(defun gg/bit-bar-timer ()
  (cond ((not (featurep 'org-clock)) "clock not loaded | color=red")
        ((org-clocking-p) (format "[%s] (%s)[%s]" (org-duration-from-minutes (org-clock-get-clocked-time)) org-clock-heading (org-pomodoro-format-seconds)))
        ((org-pomodoro-active-p) (format "%s~%s | color=teal" org-pomodoro-state (org-pomodoro-format-seconds)))
        (t "not clocking | color=red")))

(remove-hook! 'find-file-not-found-functions #'doom-create-missing-directories-h)
(add-hook! 'find-file-not-found-functions
  (defun gg/create-missing-directories ()
    (make-directory (file-name-directory buffer-file-name) 'parents)))

(defun gg/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'doom-solarized-light t))
    ('dark (load-theme 'doom-wilmersdorf t))))
(defun gg/load-only-theme (theme)
  "Disable all themes and then load a single theme interactively."
  (while custom-enabled-themes
    (disable-theme (car custom-enabled-themes)))
  (load-theme theme t))
(defun gg/pomodoro-theme ()
  (cl-case org-pomodoro-state
    (:start (gg/load-only-theme 'doom-outrun-electric))
    (:pomodoro (gg/load-only-theme 'doom-outrun-electric))
    ;; (:overtime nil)
    (:killed (gg/load-only-theme 'doom-solarized-light))
    (:none (gg/load-only-theme 'doom-solarized-light))
    (:short-break (gg/load-only-theme 'doom-solarized-dark))
    (:long-break (gg/load-only-theme 'doom-solarized-dark))
    ;; (:tick nil)
    (t (error "Unknown org-pomodoro state: %S" org-pomodoro-state))))

;; (add-hook 'ns-system-appearance-change-functions #'gg/apply-theme)

(add-hook 'org-pomodoro-finished-hook #'gg/pomodoro-theme)
(add-hook 'org-pomodoro-started-hook #'gg/pomodoro-theme)
(add-hook 'org-pomodoro-overtime-hook #'gg/pomodoro-theme)
(add-hook 'org-pomodoro-killed-hook #'gg/pomodoro-theme)
(add-hook 'org-pomodoro-break-finished-hook #'gg/pomodoro-theme)
;; (add-hook org-pomodoro-long-break-finished-hook #'gg/pomodoro-theme)
;; (add-hook org-pomodoro-short-break-finished-hook #'gg/pomodoro-theme)


(setq mac-command-modifier 'super
      mac-option-modifier 'meta
      mac-right-command-modifier 'super
      mac-right-option-modifier 'meta
      doom-leader-alt-key "C-M-SPC"
      doom-localleader-alt-key "M-SPC")

;; (after! flyspell
;;   (setq flyspell-lazy-idle-seconds 2))

(after! company
  (setq company-idle-delay 1
        company-tooltip-idle-delay 1.5))

;; (setq next-error-message-highlight t)

(add-to-list 'default-frame-alist '(height . 48))
(add-to-list 'default-frame-alist '(width . 160))
;; (elp-instrument-package "org")

(defun gg/random-note (&optional other-window)
  (interactive)
  (org-roam-node-random other-window (lambda (node) (not (org-roam-dailies--daily-note-p (org-roam-node-file node))))))

(map! :map doom-leader-notes-map
      :desc "Random non-diary"   "r A" #'gg/random-note)
(setq-hook! 'yaml-mode-hook +format-with nil)
(after! eshell
  (set-popup-rule! "*doom:eshell-popup:*" :width 80 :vslot -4 :select t :quit nil :ttl 0 :side 'right))
(define-derived-mode helm-mode yaml-mode "helm"
  "Major mode for editing kubernetes helm templates")
(add-to-list 'magic-mode-alist '("{{" . helm-mode))
(setq lsp-yaml-schemas '(
                         (kubernetes . "/*")
                         ("https://json.schemastore.org/kustomization.json" . "kustomization.yaml")))

(setq lsp-yaml-schemas nil)
(add-function :after after-focus-change-function
              (lambda () (org-save-all-org-buffers)))
(setq org-pomodoro-length 20
      org-pomodoro-long-break-frequency 4)
(sp-pair "{" "}")
(sp-pair "{{" "}}")
(after! projectile
  (setq projectile-enable-caching nil)
  ;; jdtls/lsp-java writes an Eclipse `.project` file into every Maven module it
  ;; imports (core/, core/core-web/, platform/, ...). Doom registers `.project`
  ;; as a bottom-up project-root marker, so `projectile-root-bottom-up' returns
  ;; the deepest module dir instead of the enclosing `.git' repo — the project
  ;; root jumps to whatever submodule you're editing. Drop `.project` so only
  ;; real VCS roots (and `.projectile') count, giving a holistic repo-wide view.
  (setq projectile-project-root-files-bottom-up
        (remove ".project" projectile-project-root-files-bottom-up))
  (projectile-update-project-type
   'maven
   :marker-files "pom.xml"
   :project-file "pom.xml")

  (defun gg/ssh-config-host-aliases ()
    "Return concrete host aliases from ~/.ssh/config."
    (let ((ssh-config (expand-file-name "~/.ssh/config"))
          hosts)
      (when (file-readable-p ssh-config)
        (with-temp-buffer
          (insert-file-contents ssh-config)
          (goto-char (point-min))
          (while (re-search-forward
                  "^[[:blank:]]*[Hh][Oo][Ss][Tt][[:blank:]]+\\(.+\\)$"
                  nil t)
            (dolist (host (split-string (match-string-no-properties 1) "[[:blank:]]+" t))
              (unless (string-match-p "[*?!]" host)
                (push host hosts))))))
      (delete-dups (nreverse hosts))))

  (defun gg/tramp-project-path (host dir &optional sudo)
    "Build a TRAMP project path for HOST and DIR.
When SUDO is non-nil, use ssh|sudo multi-hop."
    (let* ((remote-dir (if (string-prefix-p "/" dir) dir (concat "/" dir)))
           (clean-dir (directory-file-name remote-dir)))
      (if sudo
          (format "/ssh:%s|sudo:%s:%s/" host host clean-dir)
        (format "/ssh:%s:%s/" host clean-dir))))

  (defun gg/projectile-add-remote-project (host dir &optional no-sudo)
    "Add HOST:DIR to Projectile and switch to it.
Default behavior uses sudo multi-hop. Use prefix argument NO-SUDO to skip sudo."
    (interactive
     (let* ((hosts (gg/ssh-config-host-aliases))
            (host (completing-read
                   "SSH host alias: "
                   hosts nil nil nil nil (car hosts)))
            (dir (read-string "Remote directory: " "/")))
       (list host dir current-prefix-arg)))
    (let ((project (gg/tramp-project-path host dir (not no-sudo))))
      (projectile-add-known-project project)
      (projectile-save-known-projects)
      (projectile-switch-project-by-name project)))

  (defun gg/projectile-switch-project-magit ()
    "Switch to a project and open its Magit status buffer.
Useful when opening a project just to pull/review changes rather
than to visit a particular file."
    (interactive)
    ;; Override the post-switch function (find-file prompt by default) rather
    ;; than `projectile-switch-project-action', which Doom's :ui workspaces
    ;; module owns — replacing the action skips workspace creation, so the
    ;; project wouldn't show up in `SPC TAB'.
    (let ((+workspaces-switch-project-function #'magit-status))
      (call-interactively #'projectile-switch-project)))

  (map! :leader
        :desc "Add/switch remote project"
        "p R" #'gg/projectile-add-remote-project
        :desc "Switch project to magit"
        "p m" #'gg/projectile-switch-project-magit))

;;; Git worktrees
;;
;; Claude Code sessions work in git worktrees so concurrent threads cannot
;; collide on a branch — git refuses to check the same branch out twice. Its
;; EnterWorktree places them at `<repo>/.claude/worktrees/<name>' and there is no
;; setting for the location, so the config below meets it there rather than
;; fighting it. See ~/Developer/agent-memory/docs/specs/ for the design.

(after! magit

  ;; `magit-insert-worktrees' ships with magit but is NOT in the default
  ;; `magit-status-sections-hook', so worktrees are invisible in magit-status.
  ;; This is load-bearing rather than cosmetic: visiting a worktree is what
  ;; registers it with Projectile, so without the section there is no cheap way
  ;; in. Positioned right after the status headers, i.e. top of the buffer.
  (magit-add-section-hook 'magit-status-sections-hook
                          #'magit-insert-worktrees
                          #'magit-insert-status-headers
                          'append)

  ;; magit's shipped default is `magit-read-worktree-directory-sibling', which
  ;; would scatter hand-made worktrees beside the repo while agent-made ones live
  ;; inside it. Point `%' at the same directory EnterWorktree uses.
  (defun gg/magit-read-worktree-directory-claude (prompt branch)
    "Read a new worktree directory under the main checkout's `.claude/worktrees/'.
Suitable as `magit-read-worktree-directory-function'.  PROMPT is passed
through to `read-directory-name'.  BRANCH seeds the initial input, with
slashes replaced by dashes, matching magit's own convention.

Rooted at the *main* checkout rather than at `magit-toplevel', so creating
a worktree while already inside one does not nest them.  Uses
--git-common-dir, the same signal that distinguishes a linked worktree
from the main checkout."
    (let* ((common (magit-git-string "rev-parse" "--path-format=absolute"
                                     "--git-common-dir"))
           (main   (file-name-directory (directory-file-name common)))
           (base   (expand-file-name ".claude/worktrees/" main)))
      (make-directory base t)
      (read-directory-name prompt base nil nil
                           (and branch (string-replace "/" "-" branch)))))

  (setq magit-read-worktree-directory-function
        #'gg/magit-read-worktree-directory-claude))

(after! projectile
  (defun gg/projectile-switch-worktree ()
    "Switch to another worktree of the current repository.
Lists the repo's other worktrees, adds the chosen one to Projectile's
known projects, then switches to it in its own workspace with Magit
status open.  Works in both directions: from the main checkout out to a
worktree, and from a worktree back to the main checkout."
    (interactive)
    (let* ((here (magit-toplevel))
           (others (cl-remove here (magit-list-worktrees)
                              :test #'file-equal-p :key #'car))
           (candidates
            (mapcar (lambda (wt)
                      (let* ((path   (nth 0 wt))
                             (commit (nth 1 wt))
                             (branch (nth 2 wt))
                             (rel    (file-relative-name path here)))
                        (cons (format "%-30s %s"
                                      (or branch
                                          (if commit
                                              (concat "detached " (substring commit 0 7))
                                            "detached"))
                                      ;; A relative path is the useful label going
                                      ;; *into* a nested worktree, but coming back
                                      ;; out it degenerates to "../../../" — show
                                      ;; the abbreviated absolute path instead.
                                      (if (string-prefix-p ".." rel)
                                          (abbreviate-file-name path)
                                        rel))
                              path)))
                    others)))
      (unless candidates
        (user-error "No other worktrees in %s" (abbreviate-file-name here)))
      (let* ((choice (completing-read "Switch to worktree: " candidates nil t))
             (path (cdr (assoc choice candidates))))
        (projectile-add-known-project path)
        (projectile-save-known-projects)
        ;; Override the post-switch function, NOT
        ;; `projectile-switch-project-action' — in Doom that action *is*
        ;; `+workspaces-switch-to-project-h', the workspace creator, so
        ;; replacing it would skip `SPC TAB' registration entirely.
        (let ((+workspaces-switch-project-function #'magit-status))
          (projectile-switch-project-by-name path)))))

  (map! :leader
        :desc "Switch worktree"
        "p w" #'gg/projectile-switch-worktree))
;; (map!
;;  :map sops-mode-map
;;  :bind (("C-c C-c" . sops-save-file)
;;         ("C-c C-k" . sops-cancel)
;;         ("C-c C-d" . sops-edit-file)))

;; Org-drill — spaced repetition flashcards
(use-package! org-drill
  :after org
  :config
  (setq org-drill-scope 'directory       ; drill all files in the same directory
        org-drill-add-random-noise-to-intervals-p t
        org-drill-learn-fraction 0.3))    ; show 30% new cards per session

(map! :leader
      :desc "Drill session" "n D" #'org-drill)
