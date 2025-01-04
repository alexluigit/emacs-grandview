;;; early-init.el --- -*- lexical-binding: t -*-

(when (native-comp-available-p)
  (setq native-comp-eln-load-path
        (append (list (expand-file-name "~/.cache/emacs/eln-cache/"))
                (delete (expand-file-name "eln-cache/" user-emacs-directory)
                        native-comp-eln-load-path))
        native-comp-async-report-warnings-errors 'silent))

(setq
 gc-cons-threshold most-positive-fixnum ; Inhibit garbage collection during startup
 package-quickstart nil ; Prevent package.el loading packages prior to their init-file
 package-enable-at-startup t
 package-archives
 '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
   ("nongnu" . "https://elpa.nongnu.org/nongnu/")
   ("melpa" . "https://melpa.org/packages/"))
 package-archive-priorities '(("melpa" . 3) ("gnu-elpa" . 2) ("nongnu" . 1))
 package-install-upgrade-built-in nil
 ad-redefinition-action 'accept ; Disable warnings from legacy advice system
 inhibit-startup-message t ; Reduce noise at startup
 inhibit-startup-echo-area-message user-login-name
 inhibit-default-init t
 initial-scratch-message nil
 auto-mode-case-fold nil ; Use case-sensitive `auto-mode-alist' for performance
 fast-but-imprecise-scrolling t ; More performant rapid scrolling over unfontified regions
 ffap-machine-p-known 'reject ; Don't ping things that look like domain names
 frame-inhibit-implied-resize t ; Inhibit frame resizing for performance
 idle-update-delay 1.0  ; slow down UI updates down
 inhibit-compacting-font-caches t ; Inhibit frame resizing for performance
 read-process-output-max (* 1024 1024) ; Increase how much is read from processes in a single chunk.
 process-adaptive-read-buffering nil
 redisplay-skip-fontification-on-input t ; Inhibits it for better scrolling performance.
 command-line-x-option-alist nil ; Remove irreleant command line options for faster startup
 select-active-regions 'only ; Emacs hangs when large selections contain mixed line endings.
 auto-save-list-file-prefix nil ; Disable auto-save
 create-lockfiles nil ; Disable lockfiles
 make-backup-files nil ; Disable backup files
 vc-follow-symlinks t ; Do not ask about symlink following
 user-emacs-directory (expand-file-name "~/.cache/emacs/") ; No littering
 package-user-dir (locate-user-emacs-file "elpa")
 custom-file (concat user-emacs-directory "custom.el") ; Place all "custom" code in a temporary file
 use-short-answers t ; y/n for yes/no
 safe-local-variable-values
 '((eval . (ignore-errors (grandview-setup-literate-file)))))

(setq ring-bell-function #'ignore)    ; Do NOT ring the bell
(load custom-file 'noerror 'silent)   ; Load user's customization
(tool-bar-mode -1)                    ; Disable toolbar
(tooltip-mode -1)                     ; Disable tooltips
(menu-bar-mode -1)                    ; Disable menu bar
(scroll-bar-mode -1)                  ; Disable scroll bar
