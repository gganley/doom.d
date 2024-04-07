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
(setq doom-theme 'doom-one
      doom-font "Fira Code-10")
(require 'org-indent)
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "/Users/gregory.ganley/Documents/org-roam"
      org-roam-directory "/Users/gregory.ganley/Documents/org-roam"
      org-refile-allow-creating-parent-nodes 'confirm
      org-default-notes-file (expand-file-name "notes.org" org-directory)
      +org-capture-notes-file (expand-file-name "notes.org" org-directory)
      +org-capture-journal-file (expand-file-name "journal.org" org-directory))
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

(defun gg/pomodoro-theme ()
  (cl-case org-pomodoro-state
    (:start (load-theme 'doom-one t))
    (:pomodoro (load-theme 'doom-one t))
    ;; (:overtime nil)
    (:killed (load-theme 'doom-solarized-light t))
    (:none (load-theme 'doom-solarized-light t))
    (:short-break (load-theme 'doom-solarized-dark t))
    (:long-break (load-theme 'doom-solarized-dark t))
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

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))
(setq-hook! 'yaml-mode-hook +format-with :none)
(after! eshell
  (set-popup-rule! "*doom:eshell-popup:*" :width 80 :vslot -4 :select t :quit nil :ttl 0 :side 'right))
