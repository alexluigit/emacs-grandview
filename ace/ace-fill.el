(defgroup ace/fill ()
  "Tweak for filling paragraphs."
  :group 'fill)

(defcustom ace/fill-default-column 80
  "Default width for `fill-column'."
  :type 'integer
  :group 'ace/fill)

(defcustom ace/fill-prog-mode-column 100
  "`prog-mode' width for `fill-column'.
Also see `ace/fill-default-column'."
  :type 'integer
  :group 'ace/fill)

(defun ace/fill--fill-prog ()
  "Set local value of `fill-column' for programming modes.
Meant to be called via `prog-mode-hook'."
  (setq-local fill-column ace/fill-prog-mode-column))

;;;###autoload
(define-minor-mode ace/fill-fill-mode
  "Set up fill-mode and relevant variable."
  :init-value nil
  :global t
  (if ace/fill-fill-mode
      (progn
        (setq-default fill-column ace/fill-default-column)
        (add-hook 'prog-mode-hook #'ace/fill--fill-prog)
        (add-hook 'text-mode-hook #'turn-on-auto-fill))
    (setq-default fill-column 80)
    (remove-hook 'prog-mode-hook #'ace/fill--fill-prog)
    (remove-hook 'text-mode-hook #'turn-on-auto-fill)))

(provide 'ace-fill)
