;;; ~/.doom.d/autoload/toggl.el -*- lexical-binding: t; -*-

;;; package --- Summary
;;; Commentary:
;;; Code:
(require 'json)
(require 'parse-time)

(defvar doom-modeline--toggl-timer nil)
(defvar doom-modeline--toggl-format "None")
(defvar doom-modeline-toggl-interval 10)
(defvar doom-modeline-toggl t)

;;;###autoload
(defun doom-modeline-toggl-timer ()
  "Start/Stop the timer for github fetching."
  (if (timerp doom-modeline--toggl-timer)
      (cancel-timer doom-modeline--toggl-timer))
  (setq doom-modeline--toggl-timer
        (and doom-modeline-toggl
             (run-with-timer 10
                             doom-modeline-toggl-interval
                             #'get-toggl-string))))
;;;###autoload
(when (>= emacs-major-version 26)
  (add-variable-watcher
   'doom-modeline-toggl
   (lambda (_sym val op _where)
     (when (eq op 'set)
       (setq doom-modeline-toggl val)
       (doom-modeline-toggl-timer)))))

;;;###autoload
(doom-modeline-toggl-timer)

;;;###autoload
(defun my-insert-file-contents (url &optional visit beg end replace)
  (let ((buffer (url-retrieve-synchronously url 't)))
    (unless buffer (signal 'file-error (list url "No Data")))
    (with-current-buffer buffer
      ;; XXX: This is HTTP/S specific and should be moved to url-http
      ;; instead.  See bug#17549.
      (when (bound-and-true-p url-http-response-status)
        ;; Don't signal an error if VISIT is non-nil, because
        ;; 'insert-file-contents' doesn't.  This is required to
        ;; support, e.g., 'browse-url-emacs', which is a fancy way of
        ;; visiting the HTML source of a URL: in that case, we want to
        ;; display a file buffer even if the URL does not exist and
        ;; 'url-retrieve-synchronously' returns 404 or whatever.
        (unless (or visit
                    (and (>= url-http-response-status 200)
                         (< url-http-response-status 300)))
          (let ((desc (nth 2 (assq url-http-response-status url-http-codes))))
            (kill-buffer buffer)
            ;; Signal file-error per bug#16733.
            (signal 'file-error (list url desc))))))
    (url-insert-buffer-contents buffer url visit beg end replace)))

;;;###autoload
(defun get-toggl-string ()
  (let ((url-request-extra-headers `(("Authorization" . ,(concat "Basic " (base64-encode-string (concat (getenv "TOGGL_API_KEY") ":api_token")))))))
    (with-temp-buffer
      (my-insert-file-contents
       "https://www.toggl.com/api/v8/time_entries/current")
      (let* ((json-false :false)
             (data (json-read)))
        (if (cdr (car data))
            (let* ((start (cdr (assoc 'start (cdr (car data)))))
                   (description (cdr (assoc 'description (cdr (car data)))))
                   (pid (cdr (assoc 'pid (cdr (car data)))))
                   (project-name (get-project-name pid)))
              (setq doom-modeline--toggl-format (concat description " " project-name " " (format "%s" (time-duration-since start)))))
          (setq doom-modeline--toggl-format "None"))
        )
      )))

;;;###autoload
(defun get-project-name (pid)
  (let ((url-request-extra-headers `(("Authorization" . ,(concat "Basic " (base64-encode-string (concat (getenv "TOGGL_API_KEY") ":api_token")))))))
    (with-temp-buffer
      (my-insert-file-contents (concat "https://www.toggl.com/api/v8/projects/" (number-to-string pid)))
      (let* ((json-false :false)
             (data (json-read))
             (name (cdr (assoc 'name (cdr (car data))))))
        name))))

;;;###autoload
(defun time-duration-since (time)
  (let ((ISO-8601-time-format "%FT%H:%M:%S%:z")
        (time-tuple (break-apart-float (/ (time-to-seconds (time-since (parse-iso8601-time-string time))) 3600))))
    time-tuple)
  )

;;;###autoload
(defun break-apart-float (i)
  (let ((trunked (truncate i)))
    (cons trunked (truncate (* 60 (- i trunked))))))
