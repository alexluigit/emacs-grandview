;;; ale-rg.el -*- lexical-binding: t; -*-

;;;###autoload
(defun ale/rg-config ()
  (rg-define-toggle "--context 3" (kbd "C"))
  (rg-define-toggle "-A 5" (kbd "A")))

(provide 'ale-rg)
