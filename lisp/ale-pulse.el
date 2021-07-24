(require 'pulse)

(defgroup ale/pulse ()
  "Extensions for `pulse.el'."
  :group 'editing)

(defcustom ale/pulse-pulse-command-list
  '(recenter-top-bottom reposition-window other-window ale/window-select-by-index)
  "Commands that should automatically `ale/pulse-pulse-line'.
You must restart function `ale/pulse-advice-commands-mode' for
changes to take effect."
  :type 'list
  :group 'ale/pulse)

(defface ale/pulse-line
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#8eecf4")
    (t :inverse-video t :background "#004065"))
  "Default face for `ale/pulse-pulse-line'."
  :group 'ale/pulse)

;;;###autoload
(defun ale/pulse-pulse-line (&optional face)
  "Temporarily highlight the current line with optional FACE."
  (interactive)
  (let ((beg (if (eobp)
                 (line-beginning-position 0)
               (line-beginning-position)))
        (end (line-beginning-position 2))
        (pulse-flag t)
        (pulse-delay .04)
        (face (or face 'ale/pulse-line)))
    (pulse-momentary-highlight-region beg end face)))

;;;###autoload
(defun ale/pulse-recentre-top ()
  "Reposition at the top and pulse line.
Add this to a hook, such as `imenu-after-jump-hook'."
  (let ((pulse-delay .05))
    (recenter 0)
    (ale/pulse-pulse-line)))

;;;###autoload
(defun ale/pulse-recentre-centre ()
  "Recentre and pulse line.
Add this to a hook, such as `imenu-after-jump-hook'."
  (let ((pulse-delay .05))
    (recenter nil)
    (ale/pulse-pulse-line)))

(autoload 'org-at-heading-p "org")
(autoload 'org-show-entry "org")
(autoload 'org-reveal "org")
(autoload 'outline-show-entry "outline")

;;;###autoload
(defun ale/pulse-show-entry ()
  "Reveal index at point in outline views.
To be used with a hook such as `imenu-after-jump-hook'."
  (cond
   ((and (eq major-mode 'org-mode)
         (org-at-heading-p))
    (org-show-entry)
    (org-reveal t))
   ((bound-and-true-p ale/outline-minor-mode)
    (outline-show-entry))))

(defvar ale/pulse-after-command-hook nil
  "Hook that runs after select commands.
To be used with `advice-add' after those functions declared in
`ale/pulse-pulse-command-list'.")

(defun ale/pulse-after-command (&rest _)
  "Run `ale/pulse-after-command-hook'."
  (run-hooks 'ale/pulse-after-command-hook))

;;;###autoload
(define-minor-mode ale/pulse-advice-commands-mode
  "Set up for `ale/pulse-pulse-command-list'."
  :init-value nil
  :global t
  (if ale/pulse-advice-commands-mode
      (progn
        (dolist (fn ale/pulse-pulse-command-list)
          (advice-add fn :after #'ale/pulse-after-command))
        (add-hook 'ale/pulse-after-command-hook #'ale/pulse-pulse-line))
    (dolist (fn ale/pulse-pulse-command-list)
      (advice-remove fn #'ale/pulse-after-command))
    (remove-hook 'ale/pulse-after-command-hook #'ale/pulse-pulse-line)))

(provide 'ale-pulse)
