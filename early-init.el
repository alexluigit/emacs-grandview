(defvar ale/init-dot-repo (file-truename user-emacs-directory)
  "Get dotfiles repo path before changing `user-emacs-directory'.")

(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))
(setq url-history-file (expand-file-name "url/history" user-emacs-directory))

(when (boundp 'native-comp-eln-load-path)
  (add-to-list 'native-comp-eln-load-path (concat user-emacs-directory "eln/")))
(setq native-comp-async-report-warnings-errors 'silent)

(setq package-quickstart nil)
(setq package-enable-at-startup nil)  ; Prevent package.el loading packages prior to their init-file

(setq frame-inhibit-implied-resize t) ; Don't resize frame at this stage

(tool-bar-mode -1)                    ; Disable toolbar
(tooltip-mode -1)                     ; Disable tooltips
(menu-bar-mode -1)                    ; Disable menu bar

(setq visible-bell nil)               ; Set up the visible bell
(setq use-dialog-box t)               ; Only for mouse events
(setq use-file-dialog nil)
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)
(fset 'yes-or-no-p 'y-or-n-p) ; y,n for yes,no
