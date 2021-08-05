(require 'pulse)

(defgroup ale/pulse ()
  "Extensions for `pulse.el'."
  :group 'editing)

(defcustom ale/pulse-pulse-command-list
  '(ale/window-recentre-centre
    recenter-top-bottom
    reposition-window
    other-window
    ace-select-window)
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
        (pulse-delay .05)
        (face (or face 'ale/pulse-line)))
    (pulse-momentary-highlight-region beg end face)))

;; (autoload 'org-at-heading-p "org")
;; (autoload 'org-show-entry "org")
;; (autoload 'org-reveal "org")
;; (autoload 'outline-show-entry "outline")

;; ;;;###autoload
;; (defun ale/pulse-show-entry ()
;;   "Reveal index at point in outline views.
;; To be used with a hook such as `imenu-after-jump-hook'."
;;   (cond
;;    ((and (eq major-mode 'org-mode)
;;          (org-at-heading-p))
;;     (org-show-entry)
;;     (org-reveal t))
;;    ((bound-and-true-p ale/outline-minor-mode)
;;     (outline-show-entry))))

;;;###autoload
(define-minor-mode ale/pulse-advice-commands-mode
  "Set up for `ale/pulse-pulse-command-list'."
  :init-value nil
  :global t
  (if ale/pulse-advice-commands-mode
      (progn
        (dolist (fn ale/pulse-pulse-command-list)
          (advice-add fn :after (lambda (&rest _) (interactive) (ale/pulse-pulse-line)))))
    (dolist (fn ale/pulse-pulse-command-list)
      (advice-remove fn (lambda (&rest _) (interactive) (ale/pulse-pulse-line))))))

(provide 'ale-pulse)
