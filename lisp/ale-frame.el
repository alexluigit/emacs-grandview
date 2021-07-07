(defun ale/frame-adjust-transparency (&optional percent)
  (interactive "p")
  (cond ((and current-prefix-arg (numberp current-prefix-arg))
         (set-frame-parameter (selected-frame) 'alpha `(,(* 10 percent) . 50)))
        (current-prefix-arg
         (set-frame-parameter (selected-frame) 'alpha '(96 . 50)))
        (t
         (let ((opa (car (frame-parameter nil 'alpha))))
           (if (eq opa 70)
               (set-frame-parameter (selected-frame) 'alpha '(100 . 50))
             (set-frame-parameter (selected-frame) 'alpha '(70 . 50)))))))

(provide 'ale-frame)
