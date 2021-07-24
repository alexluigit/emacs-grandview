(when (featurep 'consult) (require 'consult))
(require 'ale-pulse)

(defgroup ale/consult ()
  "Tweaks for consult.el."
  :group 'minibuffer)

(defcustom ale/consult-command-centre-list
  '(consult-line consult-mark consult-outline consult-project-imenu)
  "Commands to run `ale/consult-jump-recentre-hook'.
You must restart function `ale/consult-set-up-hooks-mode' for
changes to take effect."
  :group 'ale/consult
  :type 'list)

(defcustom ale/consult-command-top-list '()
  "Commands to run `ale/consult-jump-top-hook'.
You must restart function `ale/consult-set-up-hooks-mode' for
changes to take effect."
  :group 'ale/consult
  :type 'list)

(defvar ale/consult-jump-recentre-hook nil
  "Hook that runs after select Consult commands.
To be used with `advice-add'.")

(defvar ale/consult-jump-top-hook nil
  "Hook that runs after select Consult commands.
To be used with `advice-add'.")

(defun ale/consult-after-jump-recentre (&rest _)
  "Run `ale/consult-jump-recentre-hook'."
  (run-hooks 'ale/consult-jump-recentre-hook))

(defun ale/consult-after-jump-top (&rest _)
  "Run `ale/consult-jump-top-hook'."
  (run-hooks 'ale/consult-jump-top-hook))

;;;###autoload
(define-minor-mode ale/consult-set-up-hooks-mode
  "Set up hooks for Consult."
  :init-value nil
  :global t
  (if ale/consult-set-up-hooks-mode
      (progn
        (dolist (fn ale/consult-command-centre-list)
          (advice-add fn :after #'ale/consult-after-jump-recentre))
        (dolist (fn ale/consult-command-top-list)
          (advice-add fn :after #'ale/consult-after-jump-top))
        (add-hook 'ale/consult-jump-recentre-hook #'ale/pulse-recentre-centre)
        (add-hook 'ale/consult-jump-top-hook #'ale/pulse-recentre-top)
        (add-hook 'ale/consult-jump-top-hook #'ale/pulse-show-entry))
    (dolist (fn ale/consult-command-centre-list)
      (advice-remove fn #'ale/consult-after-jump-recentre))
    (dolist (fn ale/consult-command-top-list)
      (advice-remove fn #'ale/consult-after-jump-top))
    (remove-hook 'ale/consult-jump-recentre-hook #'ale/pulse-recentre-centre)
    (remove-hook 'ale/consult-jump-top-hook #'ale/pulse-recentre-top)
    (remove-hook 'ale/consult-jump-top-hook #'ale/pulse-show-entry)))

(provide 'ale-consult)
