;;; autoload/jumper.el --- -*- lexical-binding: t -*-

(defvar ale-jumper-cmd-alist
  '(beginning-of-buffer
    end-of-buffer
    forward-sexp
    backward-sexp
    meow-next
    meow-prev
    meow-search
    consult-outline
    consult-line
    consult-project-imenu
    avy-goto-char-timer
    er/expand-region
    xref-find-definitions)
  "A list of file, adviced function, and advice function.")

(defun ale-jumper-advice (fn &rest args)
  (let ((old-pos (point)))
    (apply fn args)
    (when (> (abs (- (line-number-at-pos old-pos) (line-number-at-pos (point)))) 1)
      (better-jumper-set-jump old-pos))))

;;;###autoload
(define-minor-mode ale-jumper-sensible-jump-mode
  "Add sensible commands to jump list."
  :init-value nil
  :global t
  :group 'convenience
  (if ale-jumper-sensible-jump-mode
      (dolist (sym ale-jumper-cmd-alist)
        (advice-add sym :around 'ale-jumper-advice))
    (dolist (sym ale-jumper-cmd-alist)
      (advice-remove sym 'ale-jumper-advice))))

(provide 'ale-jumper)
