;; no backup flie
(setq make-backup-files nil)
;; no auto-save-list
(setq auto-save-list-file-prefix nil)
;; no scratch buffer
(setq initial-scratch-message "")
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)
;; no messages buffer
(setq-default message-log-max nil)
(kill-buffer "*Messages*")
;; no completion buffer
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))
;; don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar
(setq visible-bell nil)     ; Set up the visible bell
(column-number-mode)
;; use line number but disable it for some modes
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
;; use transparent background
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 . 50)))
;; show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)
;; no more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)
