;;; core/autoload/pulse.el --- -*- lexical-binding: t -*-

(require 'pulse)

(defgroup ale-pulse ()
  "Extensions for `pulse.el'."
  :group 'editing)

(defcustom ale-pulse-pulse-command-list
  '(ale-window-recentre-centre
    recenter-top-bottom
    reposition-window
    other-window
    ace-select-window)
  "Commands that should automatically `ale-pulse-pulse-line'.
You must restart function `ale-pulse-advice-commands-mode' for
changes to take effect."
  :type 'list
  :group 'ale-pulse)

(defface ale-pulse-line
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#8eecf4")
    (t :inverse-video t :background "#004065"))
  "Default face for `ale-pulse-pulse-line'."
  :group 'ale-pulse)

;;;###autoload
(defun ale-pulse-pulse-line (&optional face kill)
  "Temporarily highlight the current line with optional FACE."
  (interactive)
  (let ((beg (if (eobp)
                 (line-beginning-position 0)
               (line-beginning-position)))
        (end (line-beginning-position 2))
        (pulse-delay .05)
        (face (or face 'ale-pulse-line)))
    (pulse-momentary-highlight-region beg end face)
    (when kill (kill-ring-save beg end))))

;;;###autoload
(defun ale-pulse-save-line ()
  "Temporarily highlight the current line and copy it."
  (interactive)
  (ale-pulse-pulse-line nil t))

;;;###autoload
(define-minor-mode ale-pulse-advice-commands-mode
  "Set up for `ale-pulse-pulse-command-list'."
  :init-value nil
  :global t
  (if ale-pulse-advice-commands-mode
      (progn
        (dolist (fn ale-pulse-pulse-command-list)
          (advice-add fn :after (lambda (&rest _) (interactive) (ale-pulse-pulse-line)))))
    (dolist (fn ale-pulse-pulse-command-list)
      (advice-remove fn (lambda (&rest _) (interactive) (ale-pulse-pulse-line))))))
