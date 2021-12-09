;;; autoload/jumper.el --- -*- lexical-binding: t -*-

(defvar ale-jumper-cmd-alist
  '(beginning-of-buffer
    end-of-buffer
    forward-sexp
    backward-sexp
    meow-next
    meow-prev
    meow-search
    avy-goto-char-timer
    er/expand-region
    xref-find-definitions
    pop-to-buffer)
  "A list of functions for `ale-jumper-sensible-jump-mode'.")

(defun ale-jumper-advice (fn &rest args)
  (let ((old-buf (current-buffer))
        (old-pos (point)))
    (apply fn args)
    (when (or (not (eq old-buf (current-buffer)))
              (> (abs (- (line-number-at-pos old-pos) (line-number-at-pos (point)))) 1))
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
