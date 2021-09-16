;;; init/tangle.el --- -*- lexical-binding: t -*-

(defvar ale-exts-dir (concat ale-cache-dir "extensions/"))
(defvar ale-full-config-org (concat ale-init-dir "ale.org"))
(defvar ale-full-config (concat ale-cache-dir "full.el"))
(defvar ale-autoload-file (concat ale-cache-dir "autoload.el"))
(defvar ale-autoload-dirs `(,(concat ale-init-dir "autoload/") ,ale-exts-dir))

(defun ale-init-tangle (&optional force)
  "doc"
  (autoload 'ale-f-read (concat ale-init-dir "autoload/elisp"))
  (autoload 'ale-init-ext-tangle-target (concat ale-init-dir "autoload/init"))
  (let* ((init-md5 (concat ale-cache-dir "init.md5"))
         (old-md5 (when (file-exists-p init-md5)
                    (ale-f-read init-md5)))
         (new-md5 (secure-hash 'md5 (ale-f-read ale-full-config-org))))
    (when (or force (not (string= old-md5 new-md5)))
      (when (file-exists-p (ale-minimal-config))
        (delete-file (ale-minimal-config)))
      (when (file-exists-p ale-full-config)
        (delete-file ale-full-config))
      (with-temp-buffer
        (erase-buffer)
        (insert new-md5)
        (write-region (point-min) (point-max) init-md5))
      (require 'ob-tangle)
      (org-babel-tangle-file ale-full-config-org ale-full-config))))

(defun ale-init-autoload-files ()
  (require 'cl-lib)
  (cl-loop for dir in ale-autoload-dirs
           append (directory-files-recursively dir "\\.el$") into files
           append files))

(defun ale-init-gen-autoloads (&optional force)
  "Generate core autoload files."
  (require 'autoload)
  (autoload 'ale-f-read (concat ale-init-dir "autoload/elisp"))
  (let* ((autoload-md5 (concat ale-cache-dir "autoload.md5"))
         (old-md5 (when (file-exists-p autoload-md5)
                    (ale-f-read autoload-md5)))
         (files-as-str (with-temp-buffer
                         (dolist (file (ale-init-autoload-files))
                           (insert-file-contents file))
                         (buffer-string)))
         (new-md5 (secure-hash 'md5 files-as-str)))
    (when (or force (not (string= old-md5 new-md5)))
      (with-temp-file ale-autoload-file
        (cl-loop with generated-autoload-file = nil
                 with inhibit-message = t
                 for file in (ale-init-autoload-files)
                 for generated-autoload-load-name = (file-name-sans-extension file)
                 do (autoload-generate-file-autoloads file (current-buffer))))
      (with-temp-buffer
        (erase-buffer)
        (insert new-md5)
        (write-region (point-min) (point-max) autoload-md5)))))

(defun ale-minimal-config ()
  (concat ale-cache-dir "minimal.el"))

(defun ale-init-ext-tangle ()
  (concat ale-exts-dir (ale-org-custom-id-get) ".el"))

(defun ale-init-profiler ()
  "Init info with packages loaded and init time."
  (when ale-debug-p
    (let ((package-count 0)
          (time (emacs-init-time "%.3f"))
          (docstr "%d packages loaded in %ss"))
      (when (boundp 'straight--profile-cache)
        (setq package-count (+ (hash-table-size straight--profile-cache) package-count)))
      (run-with-timer 1 nil 'ale-log docstr package-count time))))

(defun ale-init-build (&optional force)
  (ale-init-tangle force)
  (ale-init-gen-autoloads force))

(add-hook 'kill-emacs-hook #'ale-init-build)

(add-hook 'emacs-startup-hook #'ale-init-profiler)

(unless (file-exists-p ale-cache-dir)
  (autoload 'ale-org-custom-id-get (concat ale-init-dir "autoload/org-id.el"))
  (make-directory ale-cache-dir)
  (ale-init-build t))
