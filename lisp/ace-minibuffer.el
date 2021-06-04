(defgroup ace/minibuffer ()
  "Extensions for the minibuffer."
  :group 'minibuffer)

(defcustom ace/minibuffer-completion-windows-regexp
  "\\*\\(Completions\\|Embark Collect \\(Live\\|Completions\\)\\)"
  "Regexp to match window names with completion candidates.
Used by `ace/minibuffer--get-completion-window'."
  :group 'ace/minibuffer
  :type 'string)

(defcustom ace/minibuffer-mini-cursors nil
  "Allow `cursor-type' to be modified in the minibuffer.
Refer to the source of `ace/minibuffer-mini-cursor' and
`ace/minibuffer-completions-cursor'"
  :group 'ace/minibuffer
  :type 'boolean)

;;;; Cursor appearance

(defun ace/minibuffer--cursor-type ()
  "Determine whether `cursor-type' is a list and return value.
If it is a list, this actually returns its car."
  (if (listp cursor-type)
      (car cursor-type)
    cursor-type))

;;;###autoload
(defun ace/minibuffer-mini-cursor ()
  "Local value of `cursor-type' for `minibuffer-setup-hook'."
  (when ace/minibuffer-mini-cursors
    (pcase (ace/minibuffer--cursor-type)
      ('hbar (setq-local cursor-type '(hbar . 8)))
      ('bar (setq-local cursor-type '(hbar . 3)))
      (_  (setq-local cursor-type '(bar . 2))))))

;;;###autoload
(defun ace/minibuffer-completions-cursor ()
  "Local value of `cursor-type' for `completion-setup-hook'."
  (when ace/minibuffer-mini-cursors
    (pcase (ace/minibuffer--cursor-type)
      ('hbar (setq-local cursor-type 'box))
      ('bar (setq-local cursor-type '(hbar . 8)))
      (_  (setq-local cursor-type '(bar . 3))))))

;;;; Minibuffer interactions

;;;###autoload
(defun ace/minibuffer--get-completion-window ()
  "Find a live window showing completion candidates."
  (get-window-with-predicate
   (lambda (window)
     (string-match-p
      ace/minibuffer-completion-windows-regexp
      (format "%s" window)))))

(provide 'ace-minibuffer)
