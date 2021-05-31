(defvar ace/init-dot-repo (file-truename user-emacs-directory)
  "Get dotfiles repo path before changing `user-emacs-directory'.")

(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))
(setq url-history-file (expand-file-name "url/history" user-emacs-directory))

(setq native-comp-eln-load-path
      (remove (expand-file-name "eln-cache/" "~/.config/emacs/") native-comp-eln-load-path))
(defvar comp-deferred-compilation-deny-list nil)
(push (expand-file-name "eln-cache/" user-emacs-directory) native-comp-eln-load-path)
(setq native-comp-async-report-warnings-errors 'silent)

(setq package-quickstart nil)
(setq package-enable-at-startup nil)  ; Prevent package.el loading packages prior to their init-file

(setq frame-inhibit-implied-resize t) ; Don't resize frame at this stage

(scroll-bar-mode -1)                  ; Disable scrollbar
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
