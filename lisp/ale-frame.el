(defun ale/frame-adjust-transparency (&optional percent)
  (interactive "p")
  (cond ((and current-prefix-arg (numberp current-prefix-arg))
         (set-frame-parameter (selected-frame) 'alpha `(,(* 10 percent) . 50)))
        (current-prefix-arg
         (set-frame-parameter (selected-frame) 'alpha '(80 . 50)))
        (t
         (let ((opa (car (frame-parameter nil 'alpha)))
               (low 60) (high 100))
           (if (eq opa low)
               (set-frame-parameter (selected-frame) 'alpha `(,high . 50))
             (set-frame-parameter (selected-frame) 'alpha `(,low . 50)))))))

(provide 'ale-frame)
