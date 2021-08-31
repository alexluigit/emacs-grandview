;;; init.el --- -*- lexical-binding: t -*-

(defvar ale-debug-p t)
(defvar ale-init-directory (file-name-directory user-init-file))
(defvar ale-cache-dir (concat user-emacs-directory "ale/"))
(defvar ale-extensions-dir (concat ale-cache-dir "extensions/"))
(defvar ale-minimal-config (concat ale-cache-dir "minimal.el"))
(defvar ale-full-config-org (concat ale-init-directory "ale.org"))
(defvar ale-full-config (concat ale-cache-dir "full.el"))
(defvar ale-autoload-file (concat ale-cache-dir "autoload.el"))
(defvar ale-autoload-directories `(,(concat ale-init-directory "autoload/")
                                   ,ale-extensions-dir))
(defvar ale-proxy '(("https" . "127.0.0.1:1088")))

(unless (file-exists-p ale-cache-dir)
  (make-directory ale-cache-dir)
  (autoload 'ale-bootstrap-build (concat ale-init-directory "autoload/bootstrap"))
  (ale-bootstrap-build t))

(load ale-autoload-file nil t)
(load (concat ale-init-directory "autoload/package.el") nil t)
(dolist (file (directory-files-recursively
               (concat ale-init-directory "core") "\\.el$"))
  (load file nil t))
(load ale-minimal-config nil t)
(when (daemonp) (load ale-full-config nil t))
