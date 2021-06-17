(defun ace/atom-forward-char ()
  (interactive)
  (when (< (point) (point-max))
    (forward-char)))

(defun ace/atom-backward-char ()
  (interactive)
  (let* ((pos (1- (point)))
         (char-writable (get-text-property pos 'read-only)))
    (unless char-writable
      (backward-char))))

(provide 'ace-atom)
