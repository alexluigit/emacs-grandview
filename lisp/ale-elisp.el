(eval-when-compile (require 'subr-x))

(defun ale/elisp-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defun ale/elisp-show-messages ()
  "Show *Messages* buffer."
  (interactive)
  (if-let ((win (get-buffer-window "*Messages*")))
      (delete-window win)
    (display-buffer-in-side-window (get-buffer "*Messages*") '((side . right)))))

(defun ale/elisp-erase-messages ()
  (interactive)
  (let ((inhibit-read-only t))
    (with-current-buffer "*Messages*" (erase-buffer))))

(provide 'ale-elisp)
