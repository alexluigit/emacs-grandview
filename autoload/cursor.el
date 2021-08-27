;;; autoload/cursor.el --- -*- lexical-binding: t -*-

(defvar ale-term-cursor-seq
  '((prefix     . "\e[")
    (suffix     . " q")
    (box-blink  . "1")
    (box        . "2")
    (hbar-blink . "3")
    (hbar       . "4")
    (bar-blink  . "5")
    (bar        . "6"))
  "escape sequence to generate cursor shape in terminal.")

;;;###autoload
(defun ale-set-cursor (&rest _)
  "Set cursor type for terminal emacs."
  (unless (display-graphic-p)
    (let ((shape (if (listp cursor-type) (car cursor-type) cursor-type)))
      (unless (member shape '(box bar hbar))
        (setq shape 'box))
      (let-alist ale-term-cursor-seq
        (send-string-to-terminal
         (cond ((eq shape 'box) (concat .prefix (if blink-cursor-mode .box-blink .box) .suffix))
               ((eq shape 'bar) (concat .prefix (if blink-cursor-mode .bar-blink .bar) .suffix))
               ((eq shape 'hbar) (concat .prefix (if blink-cursor-mode .hbar-blink .hbar) .suffix))
               (t "")))))))
