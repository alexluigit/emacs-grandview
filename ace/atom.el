(defgroup atom ()
  "Auxiliary functions for my dotemacs."
  :group 'editing)

;;;###autoload
(defun atom/number-even-p (n)
  "Test if N is an even number."
  (if (numberp n)
      (= (% n 2) 0)
    (error "%s is not a number" n)))

;;;###autoload
(defun atom/number-integer-p (n)
  "Test if N is an integer."
  (if (integerp n)
      n
    (error "%s is not an integer" n)))

;;;###autoload
(defun atom/number-negative (n)
  "Make N negative."
  (if (numberp n)
      (string-to-number (format "-%d" n)) ; TODO: better way?
    (error "%s is not a number" n)))

;;;###autoload
(defun atom/minor-modes-active ()
  "Return list of active minor modes for the current buffer."
  (let ((active-modes))
    (mapc (lambda (m)
            (when (and (boundp m) (symbol-value m))
              (push m active-modes)))
          minor-mode-list)
    active-modes))

(provide 'atom)
