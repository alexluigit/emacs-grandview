;;;###autoload
(defun ace/complete-append-metadata (category candidates)
  "Helper for generate a lambda function for `completing-read' that
will append CAETGORY metadata for CANDIDATES."
  (lexical-let ((category category) (candidates candidates))
    (lambda (input pred action)
      (if (eq action 'metadata)
          `(metadata (category . ,category))
        (complete-with-action action candidates input pred)))))

;;;###autoload
(defun ace/complete-word-ispell ()
  "Completes the symbol at point based on entries in the
dictionary"
  (interactive)
  (let* ((word (thing-at-point 'symbol t))
         (boundaries (bounds-of-thing-at-point 'symbol))
         (start (car boundaries))
         (end (cdr boundaries))
         (words (ispell-lookup-words word)))
    (let ((selection (completing-read "Words: " words)))
      (if selection
          (progn
            (delete-region start end)
            (insert selection))))))

(provide 'ace-complete)
