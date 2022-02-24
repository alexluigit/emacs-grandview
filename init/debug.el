;;; init/debug.el --- -*- lexical-binding: t -*-

(defun ale-log (format-string &rest args)
  "Log to *Messages* if `ale-debug-p' is on.
Does not display text in echo area, but still logs to *Messages*. Accepts the
same arguments as `message'."
  (when ale-debug-p
    (let ((inhibit-message (active-minibuffer-window))
          (str (concat (propertize "ALE " 'face 'font-lock-comment-face)
                       format-string)))
      (apply 'message (push str args)))))

(defun ale-show-messages (&optional erase)
  "Show *Messages* buffer in other frame.
If ERASE is non-nil, erase the buffer before switching to it."
  (interactive "P")
  (when erase
    (let ((inhibit-read-only t))
      (with-current-buffer "*Messages*" (erase-buffer))))
  (let ((win (get-buffer-window "*Messages*" t))
        (after-make-frame-functions nil))
    (if (window-live-p win)
        (delete-frame (window-frame win))
      (with-selected-frame (make-frame)
        (set-window-parameter (selected-window) 'no-other-window t)
        (switch-to-buffer "*Messages*")))))

(defun ale-debug-profiler ()
  "Init info with packages loaded and init time."
  (when ale-debug-p
    (let ((package-count 0)
          (time (emacs-init-time "%.3f"))
          (docstr "%d packages loaded in %ss"))
      (when (boundp 'straight--profile-cache)
        (setq package-count (+ (hash-table-size straight--profile-cache) package-count)))
      (run-with-timer 1 nil 'ale-log docstr package-count time))))

(add-hook 'emacs-startup-hook #'ale-debug-profiler)
