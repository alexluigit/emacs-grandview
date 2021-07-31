(defvar ale/better-jumper-cmd-alist
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

(defun ale/better-jumper-advice (fn &rest args)
  (let ((old-pos (point)))
    (apply fn args)
    (when (> (abs (- (line-number-at-pos old-pos) (line-number-at-pos (point)))) 1)
      (better-jumper-set-jump old-pos))))

(dolist (sym ale/better-jumper-cmd-alist)
  (advice-add sym :around 'ale/better-jumper-advice))

(provide 'ale-jumper)
