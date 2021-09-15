;;; autoload/dired.el --- -*- lexical-binding: t -*-

;;;###autoload
(defun ale-dired-jump () nil)

(defcustom ale-dired-routes '()
  "doc"
  :group 'files :type 'list
  :set
  (lambda (k v)
    `(setq ,k v)
    (eval `(transient-define-prefix ale-dired-jump ()
             ["Go to Directory: "
              ,@(cl-loop for (key desc path) in v
                         collect (list key desc `(lambda () (interactive) (ale-files-find-dir ,path))))]))))

;;;###autoload
(defun ale-dired-rename-space-to-underscore ()
  "Rename current or marked files by replacing space to underscore."
  (interactive)
  (require 'dired-aux)
  (if (or (eq major-mode 'dired-mode)
          (eq major-mode 'danger-mode))
      (let ((markedFiles (dired-get-marked-files )))
        (mapc (lambda (x)
                (when (string-match " " x )
                  (dired-rename-file x (replace-regexp-in-string " " "_" x) nil)))
              markedFiles)
        (if (eq major-mode 'danger-mode)
            (danger-refresh)
          (revert-buffer)))
    (user-error "Not in dired/danger")))

;;;###autoload
(defun ale-dired-file-rename-eol ()
  "Rename current file from end of line."
  (interactive)
  (end-of-line)
  (wdired-change-to-wdired-mode)
  (when (featurep 'meow) (meow-append)))
