;;; autoload/frame.el --- -*- lexical-binding: t -*-

;;;###autoload
(defun ale-frame-adjust-transparency (&optional percent)
  (interactive "P")
  (cond ((or (and percent (not current-prefix-arg))
             (numberp percent))
         (set-frame-parameter (selected-frame) 'alpha `(,(* 10 percent) . 50)))
        ((equal current-prefix-arg '(4))
         (set-frame-parameter (selected-frame) 'alpha '(80 . 50)))
        (t
         (let ((opa (car (frame-parameter nil 'alpha)))
               (low 60) (high 100))
           (if (eq opa low)
               (set-frame-parameter (selected-frame) 'alpha `(,high . 50))
             (set-frame-parameter (selected-frame) 'alpha `(,low . 50)))))))
