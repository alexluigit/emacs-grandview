;;; ale-helpful.el --- -*- lexical-binding: t -*-

(defvar ale-helpful-initialized nil)

;;;###autoload
(defun ale-helpful-mode-hook ()
  ;; FIXME: A better way?
  (setq ale-helpful-initialized nil)
  (advice-add 'find-file :before
              (lambda (&rest _)
                (when (and (not ale-helpful-initialized) (derived-mode-p 'helpful-mode))
                  (switch-to-buffer "*scratch*")
                  (switch-to-prev-buffer)
                  (setq ale-helpful-initialized t))))
  (visual-line-mode))

(provide 'ale-helpful)
