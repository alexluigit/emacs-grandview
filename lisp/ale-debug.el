(eval-when-compile (require 'subr-x))

(defun ale/debug-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defun ale/debug-show-messages ()
  "Show *Messages* buffer."
  (interactive)
  (if-let ((win (get-buffer-window "*Messages*")))
      (delete-window win)
    (display-buffer-in-side-window (get-buffer "*Messages*") '((side . right)))))

(defun ale/debug-erase-messages ()
  (interactive)
  (let ((inhibit-read-only t))
    (with-current-buffer "*Messages*" (erase-buffer))))

(provide 'ale-debug)
