;;; core/autoload/bootstrap.el --- -*- lexical-binding: t -*-

(defun ale-bootstrap-tangle (&optional force)
  "doc"
  (autoload 'ale-f-read (concat ale-init-directory "core/autoload/elisp"))
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

(defun ale-bootstrap-gen-autoloads ()
  "Generate core autoload files."
  (require 'cl-lib)
  (require 'autoload)
  (with-temp-file ale-extension-autoloads-file
    (cl-loop with generated-autoload-file = nil
             with inhibit-message = t
             for file in (cl-loop
                          for dir in ale-extension-directory
                          append (directory-files-recursively dir "\\.el$") into files
                          append files)
             for generated-autoload-load-name = (file-name-sans-extension file)
             do (autoload-generate-file-autoloads file (current-buffer)))))

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
