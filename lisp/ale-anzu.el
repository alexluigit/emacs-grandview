;;; ale-anzu.el --- -*- lexical-binding: t -*-

(defun ale/anzu-query-replace (&optional arg)
  (interactive "P")
  (if arg
      (progn
        (run-with-timer 0.05 nil 'yank)
        (anzu-query-replace nil))
    (if (region-active-p)
        (progn
          (call-interactively 'kill-ring-save)
          (exchange-point-and-mark)
          (deactivate-mark t)
          (run-with-timer 0.05 nil 'yank)
          (anzu-query-replace nil))
      (anzu-query-replace-at-cursor))))

(provide 'ale-anzu)
