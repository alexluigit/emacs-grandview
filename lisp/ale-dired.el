;;; ale-dired.el --- -*- lexical-binding: t -*-

(defun ale/dired-rename-space-to-underscore ()
  "Rename current or marked files by replacing space to underscore."
  (interactive)
  (require 'dired-aux)
  (if (or (eq major-mode 'dired-mode)
          (eq major-mode 'lf-mode))
      (let ((markedFiles (dired-get-marked-files )))
        (mapc (lambda (x)
                (when (string-match " " x )
                  (dired-rename-file x (replace-regexp-in-string " " "_" x) nil)))
              markedFiles)
        (if (eq major-mode 'lf-mode)
            (lf-refresh)
          (revert-buffer)))
    (user-error "Not in dired/lf")))

(defun ale/dired-file-rename-eol ()
  "Rename current file from end of line."
  (interactive)
  (end-of-line)
  (wdired-change-to-wdired-mode)
  (when (featurep 'meow) (meow-append)))

(provide 'ale-dired)
