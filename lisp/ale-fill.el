(defgroup ale/fill ()
  "Tweak for filling paragraphs."
  :group 'fill)

(defcustom ale/fill-default-column 80
  "Default width for `fill-column'."
  :type 'integer
  :group 'ale/fill)

(defcustom ale/fill-prog-mode-column 100
  "`prog-mode' width for `fill-column'.
Also see `ale/fill-default-column'."
  :type 'integer
  :group 'ale/fill)

(defun ale/fill--fill-prog ()
  "Set local value of `fill-column' for programming modes.
Meant to be called via `prog-mode-hook'."
  (setq-local fill-column ale/fill-prog-mode-column))

;;;###autoload
(define-minor-mode ale/fill-fill-mode
  "Set up fill-mode and relevant variable."
  :init-value nil
  :global t
  (if ale/fill-fill-mode
      (progn
        (setq-default fill-column ale/fill-default-column)
        (add-hook 'prog-mode-hook #'ale/fill--fill-prog)
        (add-hook 'text-mode-hook #'turn-on-auto-fill))
    (setq-default fill-column 80)
    (remove-hook 'prog-mode-hook #'ale/fill--fill-prog)
    (remove-hook 'text-mode-hook #'turn-on-auto-fill)))

(provide 'ale-fill)
