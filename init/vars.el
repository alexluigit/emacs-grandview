;;; init/vars.el -*- lexical-binding: t; -*-

(defconst IS-GUI (or (daemonp) (display-graphic-p)))
(defconst INIT-DIR (file-name-directory user-init-file))

(defvar ale-debug-p nil)
(defvar ale-cache-dir (concat user-emacs-directory "ale/"))

(defcustom ale-local-leader-key "C-x x SPC"
  "Another Leader key trigger")

;; `project-prefix-map' is available in emacs > 28.
(if (boundp 'project-prefix-map)
    (defalias 'project-map project-prefix-map)
  (define-prefix-command 'project-prefix-map))

(defalias 'tab-map tab-prefix-map)
(defalias 'register-map ctl-x-r-map)

;; Open files/dirs or operate on files
(define-prefix-command 'ale-files-map)

;; `mct' is the acronym for "Minibuffer and Completions in Tandem".
(define-prefix-command 'ale-mct-map)

;; Shortcuts for org related commands
(define-prefix-command 'ale-org-map)

;; Useful utils such as format buffer, set frame opacity, etc.
(define-prefix-command 'ale-apps-map)
