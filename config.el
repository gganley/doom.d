;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(setq user-full-name "Gregory Ganley"
      user-mail-address "gganley@student.bridgew.edu"

      doom-font (font-spec :family "Fira Mono" :size 12)
      doom-big-font (font-spec :family "Fira Mono" :size 19))

(when IS-MAC
  (setq mac-command-modifier 'meta
        ns-alternate-modifier 'super
        mac-control-modifier 'control
        ns-function-modifier 'hyper
        ns-use-thin-smoothing t))

;; Evil
(setq evil-escape-key-sequence "fd")

;; LSP
(setq lsp-ui-doc-include-signature t
      lsp-ui-doc-max-height 32
      lsp-ui-doc-max-width 35
      lsp-ui-doc-use-webkit t
      lsp-ui-flycheck-enable t)

;; Org
(setq org-agenda-files '("~/org/projects.org" "~/org/inbox.org" "~/org/todo.org")
      org-agenda-prefix-format
      '((agenda . " %i %b %-12:c%?-12t% s")
        (todo . " %i %-25:b")
        (tags . " %i %-12:c")
        (search . " %i %-12:c"))
      org-agenda-sorting-strategy
      '((agenda habit-down time-up priority-down category-keep)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep))
      org-default-notes-file "/home/gganley/org/notes.org"
      org-directory "~/org/"
      org-outline-path-complete-in-steps nil
      org-refile-use-outline-path t
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)
        ("~/org/consume_later.org" :level . 1))
      org-agenda-todo-ignore-scheduled 'future)

(defun gg/get-keyboard-seq (key-list)
                               (interactive (list (read-key-sequence "Key combination: ")))
                               (format "%s" (key-description key-list)))
(after! org
  ;; Some of this was stolen from Doom Emacs' org/+capture
  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t :kill-buffer t)

          ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
          ;; {todo,notes,changelog}.org file is found in a parent directory.
          ;; Uses the basename from `+org-capture-todo-file',
          ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
          ("p" "Templates for projects")
          ("pt" "Project todo" entry  ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
          ("pn" "Project notes" entry  ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
          ("pc" "Project changelog" entry  ; {project-root}/changelog.org
           (file+headline +org-capture-project-notes-file "Unreleased")
           "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
          ("k" "Keybinding" entry (file+headline "~/org/keybindings.org" "Keybinding") "** %(call-interactively #'gg/get-keyboard-seq) .. %?\n"))))

(map! :leader
      (:prefix ("a" . "gganley")
        (:prefix ("p" . "paren")
          :desc "Wrap round" "(" #'sp-wrap-round
          :desc "Slurp" "s" #'sp-slurp-hybrid-sexp)
        (:prefix ("g" . "go")
          :desc "to current timer" "T" #'org-clock-goto)
        :desc "Clock in" "i" #'org-clock-in
        :desc "Clock out" "o" #'org-clock-out))
