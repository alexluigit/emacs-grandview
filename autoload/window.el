;;; autoload/window.el --- extensions for window.el -*- lexical-binding: t; -*-

(defgroup ale-window ()
  "Tweaks for windows."
  :group 'windows)

(defcustom ale-window-command-centre-list
  '(magit-diff-visit-file--internal
    consult-line
    consult-mark
    consult-outline
    consult-project-imenu
    consult-compile-error
    consult-org-heading)
  "Commands to run `ale-window-jump-recentre-hook'.
You must restart function `ale-window-set-up-hooks-mode' for
changes to take effect."
  :group 'ale-window
  :type 'list)

(defcustom ale-window-command-top-list '()
  "Commands to run `ale-window-jump-top-hook'.
You must restart function `ale-window-set-up-hooks-mode' for
changes to take effect."
  :group 'ale-window
  :type 'list)

;;;###autoload
(defun ale-window-recentre-top (&rest _)
  "Reposition at the top and pulse line.
Add this to a hook, such as `imenu-after-jump-hook'."
  (recenter 0))

;;;###autoload
(defun ale-window-recentre-centre (&rest _)
  "Recentre and pulse line.
Add this to a hook, such as `imenu-after-jump-hook'."
  (recenter nil))

;;;###autoload
(defun ale-split-window-right ()
  (interactive)
  (split-window-right) (other-window 1))

;;;###autoload
(defun ale-split-window-below ()
  (interactive)
  (split-window-below) (other-window 1))

;; Inspired by Pierre Neidhardt's windower:
;; https://gitlab.com/ambrevar/emacs-windower/-/blob/master/windower.el
(defvar ale--windows-current nil
  "Current window configuration.")

;;;###autoload
(defun ale--monocle-disable ()
  "Set variable `ale-simple-monocle' to nil, when appropriate.
To be hooked to `window-configuration-change-hook'."
  (when (and ale-monocle
             (not (and (featurep 'transient) (window-live-p transient--window)))
             (not (one-window-p)))
    (delete-other-windows)
    (ale-monocle -1)
    (set-window-configuration ale--windows-current)))

;;;###autoload
(add-hook 'window-configuration-change-hook #'ale--monocle-disable)

;;;###autoload
(define-minor-mode ale-monocle
  "Toggle between multiple windows and single window.
This is the equivalent of maximising a window.  Tiling window
managers such as DWM, BSPWM refer to this state as 'monocle'."
  :lighter " -M-"
  :global nil
  (let ((config ale--windows-current)
        (buf (current-buffer)))
    (if (one-window-p)
        (when config
          (set-window-configuration config))
      (setq ale--windows-current (current-window-configuration))
      (when (window-parameter nil 'window-side) (delete-window))
      (delete-other-windows)
      (switch-to-buffer buf))))

;;;###autoload
(define-minor-mode ale-window-recenter-mode
  "Set up hooks for Consult."
  :init-value nil
  :global t
  (if ale-window-recenter-mode
      (progn
        (dolist (fn ale-window-command-centre-list)
          (advice-add fn :after #'ale-window-recentre-centre))
        (dolist (fn ale-window-command-top-list)
          (advice-add fn :after #'ale-window-recentre-top)))
    (dolist (fn ale-window-command-centre-list)
          (advice-remove fn #'ale-window-recentre-centre))
    (dolist (fn ale-window-command-top-list)
          (advice-remove fn #'ale-window-recentre-top))))
