;;; init/vars.el -*- lexical-binding: t; -*-

(defconst IS-GUI (or (daemonp) (display-graphic-p)))
(defconst INIT-DIR (file-name-directory user-init-file))

;; No littering
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))

(defvar ale-debug-p nil)
(defvar ale-cache-dir (concat user-emacs-directory "ale/"))

(defvar ale-autoload-default-dir (concat ale-cache-dir "autoload/"))
(defvar ale-full-config-org (concat INIT-DIR "ale.org"))
(defvar ale-full-config (concat ale-cache-dir "full.el"))
(defun ale-minimal-config () (concat ale-cache-dir "minimal.el"))
(defvar ale-autoload-file (concat ale-cache-dir "autoload.el"))
(defvar ale-autoload-dirs `(,ale-autoload-default-dir))

(defvar ale-font-size 32)
(defvar ale-default-fonts '("Victor Mono" "Sarasa Mono SC" "Fira Code Retina"))
(defvar ale-fixed-fonts '("Sarasa Mono SC" "Fira Code Retina"))
(defvar ale-variable-fonts '("Sarasa Mono SC" "Victor Mono" "Fira Code Retina"))
(defvar ale-zh-fonts '("FZSuXinShiLiuKaiS-R-GB" "Smartisan Compact CNS" "Sarasa Mono SC" "青鸟华光简报宋二"))
(defvar ale-zh-font-scale 1.2)

(defcustom ale-files-additional-mime
  '((".ape" . "audio/ape")
    (".rmvb" . "video/rm")
    (".f4v" . "video/f4v"))
  "doc"
  :group 'files :type '(repeat cons))

(defcustom ale-files-dir-alist
  '(((title . "  Photos")       (path . "~/Pictures/"))
    ((title . "  Videos")       (path . "~/Video/"))
    ((title . "  Downloads")    (path . "~/Downloads/")))
  "doc"
  :group 'files :type '(repeat list))

(defcustom ale-files-cmd-alist
  '(("video/" ("floatwin" "-c" "mpv:emacs-mpv" "mpv" "--x11-name=emacs-mpv" "%f"))
    (("rm" "rmvb") ("floatwin" "-c" "mpv:emacs-mpv" "mpv" "--x11-name=emacs-mpv" "%f")))
  "doc"
  :group 'files :type '(alist :value-type ((choice list string) list)))

(defcustom ale-term-position-alist
  '((always . ((window-width . 0.4) (side . right))))
  "doc")

(defconst ale-files-dot-repo (getenv "DOTPATH")
  "doc")

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
