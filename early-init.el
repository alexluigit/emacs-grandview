; (setq package-enable-at-startup t) ;; Initialise installed packages
; (defvar package-quickstart)
; (setq package-quickstart t) ;; Allow loading from the package cache

(scroll-bar-mode -1)                 ; disable visible scrollbar
(tool-bar-mode -1)                   ; disable the toolbar
(tooltip-mode -1)                    ; disable tooltips
(menu-bar-mode -1)                   ; disable the menu bar

(setq visible-bell nil)              ; set up the visible bell
(setq use-dialog-box t)              ; only for mouse events
(setq use-file-dialog nil)
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-echo-area-message "alex")
(fset 'yes-or-no-p 'y-or-n-p) ; y,n for yes,no
