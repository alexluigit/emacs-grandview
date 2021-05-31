(when (featurep 'consult)
  (require 'consult))
(require 'ace-pulse)

(defgroup ace/consult ()
  "Tweaks for consult.el."
  :group 'minibuffer)

(defcustom ace/consult-command-centre-list '(consult-line consult-mark)
  "Commands to run `ace/consult-jump-recentre-hook'.
You must restart function `ace/consult-set-up-hooks-mode' for
changes to take effect."
  :group 'ace/consult
  :type 'list)

(defcustom ace/consult-command-top-list '(consult-outline)
  "Commands to run `ace/consult-jump-top-hook'.
You must restart function `ace/consult-set-up-hooks-mode' for
changes to take effect."
  :group 'ace/consult
  :type 'list)

;;;; Setup for some consult commands (TODO: needs review)

(defvar ace/consult-jump-recentre-hook nil
  "Hook that runs after select Consult commands.
To be used with `advice-add'.")

(defun ace/consult-after-jump-recentre (&rest _)
  "Run `ace/consult-jump-recentre-hook'."
  (run-hooks 'ace/consult-jump-recentre-hook))

(defvar ace/consult-jump-top-hook nil
  "Hook that runs after select Consult commands.
To be used with `advice-add'.")

(defun ace/consult-after-jump-top (&rest _)
  "Run `ace/consult-jump-top-hook'."
  (run-hooks 'ace/consult-jump-top-hook))

(defun ace/consult-recent-file ()
  "The same as `consult-recent-file' except no preview."
  (interactive)
  (find-file
   (consult--read
    (or (mapcar #'abbreviate-file-name recentf-list)
        (user-error "No recent files"))
    :prompt "Find recent file: "
    :sort nil
    :require-match t
    :category 'file
    :history 'file-name-history)))

;;;###autoload
(define-minor-mode ace/consult-set-up-hooks-mode
  "Set up hooks for Consult."
  :init-value nil
  :global t
  (if ace/consult-set-up-hooks-mode
      (progn
        (dolist (fn ace/consult-command-centre-list)
          (advice-add fn :after #'ace/consult-after-jump-recentre))
        (dolist (fn ace/consult-command-top-list)
          (advice-add fn :after #'ace/consult-after-jump-top))
        (add-hook 'ace/consult-jump-recentre-hook #'ace/pulse-recentre-centre)
        (add-hook 'ace/consult-jump-top-hook #'ace/pulse-recentre-top)
        (add-hook 'ace/consult-jump-top-hook #'ace/pulse-show-entry))
    (dolist (fn ace/consult-command-centre-list)
      (advice-remove fn #'ace/consult-after-jump-recentre))
    (dolist (fn ace/consult-command-top-list)
      (advice-remove fn #'ace/consult-after-jump-top))
    (remove-hook 'ace/consult-jump-recentre-hook #'ace/pulse-recentre-centre)
    (remove-hook 'ace/consult-jump-top-hook #'ace/pulse-recentre-top)
    (remove-hook 'ace/consult-jump-top-hook #'ace/pulse-show-entry)))

(provide 'ace-consult)