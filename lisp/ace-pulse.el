(require 'pulse)

(defgroup ace/pulse ()
  "Extensions for `pulse.el'."
  :group 'editing)

(defcustom ace/pulse-pulse-command-list
  '(recenter-top-bottom reposition-window)
  "Commands that should automatically `ace/pulse-pulse-line'.
You must restart function `ace/pulse-advice-commands-mode' for
changes to take effect."
  :type 'list
  :group 'ace/pulse)

(defface ace/pulse-line
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#8eecf4")
    (((class color) (min-colors 88) (background dark))
     :background "#004065")
    (t :inverse-video t))
  "Default face for `ace/pulse-pulse-line'."
  :group 'ace/pulse)

;;;###autoload
(defun ace/pulse-pulse-line (&optional face)
  "Temporarily highlight the current line with optional FACE."
  (interactive)
  (let ((start (if (eobp)
                   (line-beginning-position 0)
                 (line-beginning-position)))
        (end (line-beginning-position 2))
        (pulse-delay .04)
        (face (or face 'ace/pulse-line)))
    (pulse-momentary-highlight-region start end face)))

;;;###autoload
(defun ace/pulse-recentre-top ()
  "Reposition at the top and pulse line.
Add this to a hook, such as `imenu-after-jump-hook'."
  (let ((pulse-delay .05))
    (recenter 0)
    (ace/pulse-pulse-line)))

;;;###autoload
(defun ace/pulse-recentre-centre ()
  "Recentre and pulse line.
Add this to a hook, such as `imenu-after-jump-hook'."
  (let ((pulse-delay .05))
    (recenter nil)
    (ace/pulse-pulse-line)))

(autoload 'org-at-heading-p "org")
(autoload 'org-show-entry "org")
(autoload 'org-reveal "org")
(autoload 'outline-show-entry "outline")

;;;###autoload
(defun ace/pulse-show-entry ()
  "Reveal index at point in outline views.
To be used with a hook such as `imenu-after-jump-hook'."
  (cond
   ((and (eq major-mode 'org-mode)
         (org-at-heading-p))
    (org-show-entry)
    (org-reveal t))
   ((bound-and-true-p ace/outline-minor-mode)
    (outline-show-entry))))

(defvar ace/pulse-after-command-hook nil
  "Hook that runs after select commands.
To be used with `advice-add' after those functions declared in
`ace/pulse-pulse-command-list'.")

(defun ace/pulse-after-command (&rest _)
  "Run `ace/pulse-after-command-hook'."
  (run-hooks 'ace/pulse-after-command-hook))

;;;###autoload
(define-minor-mode ace/pulse-advice-commands-mode
  "Set up for `ace/pulse-pulse-command-list'."
  :init-value nil
  :global t
  (if ace/pulse-advice-commands-mode
      (progn
        (dolist (fn ace/pulse-pulse-command-list)
          (advice-add fn :after #'ace/pulse-after-command))
        (add-hook 'ace/pulse-after-command-hook #'ace/pulse-pulse-line))
    (dolist (fn ace/pulse-pulse-command-list)
      (advice-remove fn #'ace/pulse-after-command))
    (remove-hook 'ace/pulse-after-command-hook #'ace/pulse-pulse-line)))

(provide 'ace-pulse)
