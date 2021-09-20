;;; autoload/opacity.el --- -*- lexical-binding: t -*-

(defvar ale-opacity 80)
(defcustom ale-opacity-disabled-modes
  '(dired-mode image-mode minibuffer-mode pdf-view-mode)
  "A list of modes in which the global better-jumper minor mode will not be turned on."
  :group 'ale
  :type '(list symbol))

;;;###autoload
(defun ale-opacity-set (&optional percent)
  (interactive "P")
  (cond ((or (and percent (not current-prefix-arg))
             (numberp percent))
         (setq ale-opacity (* 10 percent))
         (set-frame-parameter (selected-frame) 'alpha `(,ale-opacity . 50)))
        ((equal current-prefix-arg '(4))
         (ale-opacity-default))
        (t
         (let ((opa (car (frame-parameter nil 'alpha)))
               (low 60) (high 100))
           (if (eq opa low)
               (set-frame-parameter (selected-frame) 'alpha `(,high . 50))
             (set-frame-parameter (selected-frame) 'alpha `(,low . 50)))))))

(defun ale-opacity-disable ()
  (set-frame-parameter (selected-frame) 'alpha '(100 . 50)))

(defun ale-opacity-default ()
  (set-frame-parameter (selected-frame) 'alpha `(,ale-opacity . 50)))

;;;###autoload
(defun ale-opacity-auto ()
  "Setup frame opacity according to current major-mode."
  (if (apply #'derived-mode-p ale-opacity-disabled-modes)
      (ale-opacity-disable)
    (ale-opacity-default)))

;;;###autoload
(define-minor-mode ale-opacity-auto-mode
  "Minor mode for adjusting frame opacity."
  :lighter " ale-auto-opacity"
  :group 'ale
  :global t
  (ale-opacity-default)
  (if ale-opacity-auto-mode
      (add-hook 'window-configuration-change-hook #'ale-opacity-auto)
    (remove-hook 'window-configuration-change-hook #'ale-opacity-auto)))
