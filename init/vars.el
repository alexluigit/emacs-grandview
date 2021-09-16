;;; init/vars.el -*- lexical-binding: t; -*-

(defconst IS-GUI (or (daemonp) (display-graphic-p)))
(defvar ale-debug-p nil)
(defvar ale-init-dir (file-name-directory user-init-file))
(defvar ale-cache-dir (concat user-emacs-directory "ale/"))

(defalias 'project-map project-prefix-map)
(defalias 'tab-map tab-prefix-map)
(defalias 'register-map ctl-x-r-map)
(defcustom ale-local-leader-key "C-x x SPC"
  "Another Leader key trigger")
(define-prefix-command 'ale-files-map)
(define-prefix-command 'ale-elisp-map)
(define-prefix-command 'ale-consult-map)
(define-prefix-command 'ale-utils-map)
(define-prefix-command 'ale-org-map)
