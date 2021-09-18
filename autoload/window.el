;;; autoload/window.el --- extensions for window.el -*- lexical-binding: t; -*-

(defgroup ale-window ()
  "Tweaks for windows."
  :group 'windows)

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
