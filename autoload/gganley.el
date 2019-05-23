;;; ~/.doom.d/autoload/gganley.el -*- lexical-binding: t; -*-

;;;###autoload
(defun toggl ()
  (interactive)
  (start-process-shell-command "" nil "toggl" "-d" (read-string "Decription: ") "-p" (read-string "Project: ") "-t" (read-string "Tags: ")))
