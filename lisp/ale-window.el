(defgroup ale/window ()
  "Tweaks for windows."
  :group 'windows)

(defcustom ale/window-command-centre-list
  '(magit-diff-visit-file--internal
    consult-line
    consult-mark
    consult-outline
    consult-project-imenu)
  "Commands to run `ale/window-jump-recentre-hook'.
You must restart function `ale/window-set-up-hooks-mode' for
changes to take effect."
  :group 'ale/window
  :type 'list)

(defcustom ale/window-command-top-list '()
  "Commands to run `ale/window-jump-top-hook'.
You must restart function `ale/window-set-up-hooks-mode' for
changes to take effect."
  :group 'ale/window
  :type 'list)

;;;###autoload
(defun ale/window-recentre-top (&rest _)
  "Reposition at the top and pulse line.
Add this to a hook, such as `imenu-after-jump-hook'."
  (recenter 0))

;;;###autoload
(defun ale/window-recentre-centre (&rest _)
  "Recentre and pulse line.
Add this to a hook, such as `imenu-after-jump-hook'."
  (recenter nil))

(defun ale/window-split-right ()
  (interactive)
  (split-window-right) (other-window 1))

(defun ale/window-split-below ()
  (interactive)
  (split-window-below) (other-window 1))

;;;###autoload
(define-minor-mode ale/window-recenter-mode
  "Set up hooks for Consult."
  :init-value nil
  :global t
  (if ale/window-recenter-mode
      (progn
        (dolist (fn ale/window-command-centre-list)
          (advice-add fn :after #'ale/window-recentre-centre))
        (dolist (fn ale/window-command-top-list)
          (advice-add fn :after #'ale/window-recentre-top)))
    (dolist (fn ale/window-command-centre-list)
          (advice-remove fn #'ale/window-recentre-centre))
    (dolist (fn ale/window-command-top-list)
          (advice-remove fn #'ale/window-recentre-top))))

(provide 'ale-window)
