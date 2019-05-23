
(after! doom-modeline
  (require 'json)
  (require 'parse-time)
  (defvar doom-modeline--toggl-timer nil)
  (defun doom-modeline-toggl-timer ()
    "Start/Stop the timer for github fetching."
    (if (timerp doom-modeline--toggl-timer)
        (cancel-timer doom-modeline--toggl-timer))
    (setq doom-modeline--toggl-timer
          (and doom-modeline-toggl
               (run-with-timer 30
                               doom-modeline-toggl-interval
                               #'get-toggl-string))))
  (when (>= emacs-major-version 26)
    (add-variable-watcher
     'doom-modeline-toggl
     (lambda (_sym val op _where)
       (when (eq op 'set)
         (setq doom-modeline-toggl val)
         (doom-modeline-toggl-timer)))))

  (doom-modeline-toggl-timer)


  (defun get-toggl-string ()
    (let ((url-request-extra-headers `(("Authorization" . ,(concat "Basic " (base64-encode-string (concat (getenv "TOGGL_API_KEY") ":api_token")))))))
      (with-temp-buffer
        (url-insert-file-contents
         "https://www.toggl.com/api/v8/time_entries/current")
        (let* ((json-false :false)
               (data (json-read))
               (start (cdr (assoc 'start (cdr (car data)))))
               (description (cdr (assoc 'description (cdr (car data)))))
               (pid (cdr (assoc 'pid (cdr (car data)))))
               (project-name (get-project-name pid)))
          (concat description " " project-name " " (format "%s" (time-duration-since start)))))))


  (defun get-project-name (pid)
    (let ((url-request-extra-headers `(("Authorization" . ,(concat "Basic " (base64-encode-string (concat (getenv "TOGGL_API_KEY") ":api_token")))))))
      (with-temp-buffer
        (url-insert-file-contents (concat "https://www.toggl.com/api/v8/projects/" (number-to-string pid)))
        (let* ((json-false :false)
               (data (json-read))
               (name (cdr (assoc 'name (cdr (car data))))))
          name))))

  (defun time-duration-since (time)
    (let ((ISO-8601-time-format "%FT%H:%M:%S%:z")
          (time-tuple (break-apart-float (/ (time-to-seconds (time-since (parse-iso8601-time-string time))) 3600))))
      time-tuple
      )
    )

  (defun break-apart-float (i)
    (let ((trunked (truncate i)))
      (cons trunked (truncate (* 60 (- i trunked))))))
  (doom-modeline-def-segment toggl
    "Toggl segment"
    (if (and doom-modeline-toggl
             (doom-modeline--active))
        (get-toggl-string)))

  (doom-modeline-def-modeline 'my-main
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position parrot selection-info)
    '(toggl objed-state misc-info persp-name lsp irc mu4e github debug fancy-battery minor-modes input-method buffer-encoding major-mode process vcs checker))

  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'my-main 'default))

  (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline))


(provide 'toggl)
