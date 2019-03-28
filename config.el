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
      lsp-ui-doc-max-height 25
      lsp-ui-doc-max-width 50
      lsp-ui-doc-use-webkit nil
      lsp-ui-flycheck-enable t)

;; Org
(setq org-agenda-files '("~/org/projects.org" "~/org/inbox.org" "~/org/todo.org" "~/org/personal.org")
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
      org-clock-persist 'history
      org-agenda-todo-ignore-scheduled 'future)
(org-clock-persistence-insinuate)

(defun gg/get-keyboard-seq (key-list)
                               (interactive (list (read-key-sequence "Key combination: ")))
                               (format "%s" (key-description key-list)))
(after! org-agenda
    (add-to-list 'org-agenda-custom-commands '("w" "work todos"
                                               ((tags-todo "-personal"))))
    (add-to-list 'org-agenda-custom-commands '("p" "personal project todos"
                                               ((tags-todo "personal")))))
(after! org
  (add-to-list 'org-capture-templates '("k" "Keybinding" entry (file+headline "~/org/keybindings.org" "Keybinding") "** %(call-interactively #'gg/get-keyboard-seq) .. %?\n")))
;; Doom Emacs

(map! :leader
      (:prefix ("a" . "gganley")
        (:prefix ("p" . "paren")
          :desc "Wrap round" "(" #'sp-wrap-round
          :desc "Slurp" "s" #'sp-slurp-hybrid-sexp)
        (:prefix ("g" . "go")
          :desc "to current timer" "T" #'org-clock-goto)
        :desc "Clock in" "i" #'org-clock-in
        :desc "Clock out" "o" #'org-clock-out))

;; Python

(setq python-shell-interpreter "python3"
      flycheck-python-flake8-executable "python3"
      flycheck-python-pycompile-executable "python3")
