;;; autoload/bootstrap.el --- -*- lexical-binding: t -*-

(require 'cl-lib)

(defun ale-bootstrap-autoload-files ()
  (cl-loop for dir in ale-autoload-directories
           append (directory-files-recursively dir "\\.el$") into files
           append files))

;;;###autoload
(defun ale-bootstrap-ext-tangle-target ()
  (concat ale-extensions-dir (format "%s" (org-id-get)) ".el"))

(defun ale-bootstrap-tangle (&optional force)
  "doc"
  (autoload 'ale-f-read (concat ale-init-directory "autoload/elisp"))
  (autoload 'ale-bootstrap-ext-tangle-target (concat ale-init-directory "autoload/bootstrap"))
  (let* ((bootstrap-org (concat ale-init-directory "ale.org"))
         (bootstrap-el (concat user-emacs-directory "ale-full.el"))
         (bootstrap-md5 (concat user-emacs-directory "bootstrap.md5"))
         (old-md5 (when (file-exists-p bootstrap-md5)
                    (ale-f-read bootstrap-md5)))
         (new-md5 (secure-hash 'md5 (ale-f-read bootstrap-org))))
    (when (or force (not (string= old-md5 new-md5)))
      (with-temp-buffer
        (erase-buffer)
        (insert new-md5)
        (write-region (point-min) (point-max) bootstrap-md5))
      (require 'ob-tangle)
      (org-babel-tangle-file bootstrap-org bootstrap-el))))

(defun ale-bootstrap-gen-autoloads (&optional force)
  "Generate core autoload files."
  (require 'autoload)
  (autoload 'ale-f-read (concat ale-init-directory "autoload/elisp"))
  (let* ((autoload-md5 (concat user-emacs-directory "autoload.md5"))
         (old-md5 (when (file-exists-p autoload-md5)
                    (ale-f-read autoload-md5)))
         (files-as-str (with-temp-buffer
                         (dolist (file (ale-bootstrap-autoload-files))
                           (insert-file-contents file))
                         (buffer-string)))
         (new-md5 (secure-hash 'md5 files-as-str)))
    (when (or force (not (string= old-md5 new-md5)))
      (with-temp-file ale-autoload-file
        (cl-loop with generated-autoload-file = nil
                 with inhibit-message = t
                 for file in (ale-bootstrap-autoload-files)
                 for generated-autoload-load-name = (file-name-sans-extension file)
                 do (autoload-generate-file-autoloads file (current-buffer))))
      (with-temp-buffer
        (erase-buffer)
        (insert new-md5)
        (write-region (point-min) (point-max) autoload-md5)))))

;;;###autoload
(defun ale-bootstrap-profiler ()
  "Init info with packages loaded and init time."
  (let ((package-count 0)
        (time (emacs-init-time "%.3f"))
        (docstr "%d packages loaded in %ss"))
    (when (boundp 'straight--profile-cache)
      (setq package-count (+ (hash-table-size straight--profile-cache) package-count)))
    (run-with-timer 1 nil 'ale-log docstr package-count time)))

;;;###autoload
(defun ale-bootstrap-build ()
  (ale-bootstrap-tangle)
  (ale-bootstrap-gen-autoloads))

;;;###autoload
(add-hook 'kill-emacs-hook #'ale-bootstrap-build)

;;;###autoload
(add-hook 'emacs-startup-hook #'ale-bootstrap-profiler)
