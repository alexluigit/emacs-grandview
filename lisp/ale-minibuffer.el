;;; -*- lexical-binding: t -*-

(defgroup ale/minibuffer ()
  "Extensions for the minibuffer."
  :group 'minibuffer)

(defcustom ale/minibuffer-completion-windows-regexp
  "\\*\\(Completions\\|Embark Collect \\(Live\\|Completions\\)\\)"
  "Regexp to match window names with completion candidates.
Used by `ale/minibuffer--get-completion-window'."
  :group 'ale/minibuffer
  :type 'string)

(defcustom ale/minibuffer-mini-cursors nil
  "Allow `cursor-type' to be modified in the minibuffer.
Refer to the source of `ale/minibuffer-mini-cursor' and
`ale/minibuffer-completions-cursor'"
  :group 'ale/minibuffer
  :type 'boolean)

;;;; Cursor appearance

(defun ale/minibuffer--cursor-type ()
  "Determine whether `cursor-type' is a list and return value.
If it is a list, this actually returns its car."
  (if (listp cursor-type)
      (car cursor-type)
    cursor-type))

;;;###autoload
(defun ale/minibuffer-mini-cursor ()
  "Local value of `cursor-type' for `minibuffer-setup-hook'."
  (when ale/minibuffer-mini-cursors
    (pcase (ale/minibuffer--cursor-type)
      ('hbar (setq-local cursor-type '(hbar . 8)))
      ('bar (setq-local cursor-type '(hbar . 3)))
      (_  (setq-local cursor-type '(bar . 2))))))

;;;###autoload
(defun ale/minibuffer-completions-cursor ()
  "Local value of `cursor-type' for `completion-setup-hook'."
  (when ale/minibuffer-mini-cursors
    (pcase (ale/minibuffer--cursor-type)
      ('hbar (setq-local cursor-type 'box))
      ('bar (setq-local cursor-type '(hbar . 8)))
      (_  (setq-local cursor-type '(bar . 3))))))

;;;; Minibuffer interactions

(defun ale/minibuffer--get-completion-window ()
  "Find a live window showing completion candidates."
  (get-window-with-predicate
   (lambda (window)
     (string-match-p
      ale/minibuffer-completion-windows-regexp
      (format "%s" window)))))

;;;; Minibuffer completions

(defun ale/minibuffer-append-metadata (meta completions)
  "doc"
  (let ((entry (if (functionp meta)
                   `(metadata (annotation-function . ,meta))
                 `(metadata (category . ,meta)))))
    (lambda (string pred action)
      (if (eq action 'metadata)
          entry
        (complete-with-action action completions string pred)))))

(defun ale/minibuffer-completing-read (prompt collection &rest comp-read-args)
  "Calls `completing-read' but returns the value from COLLECTION.

Simple wrapper around the `completing-read' function that assumes
the collection is either an alist, or a hash-table, and returns
the _value_ of the choice, not the selected choice."
  (cl-flet ((assoc-list-p (obj) (and (listp obj) (consp (car obj)))))
    (let* ((choice (apply #'completing-read prompt collection comp-read-args))
           (results (cond
                     ((hash-table-p collection) (gethash choice collection))
                     ((assoc-list-p collection) (alist-get choice collection nil nil 'equal))
                     (t                         choice))))
      (if (listp results) (car results) results))))

(defun ale/minibuffer--files-in-directory (dir &optional prompt)
  "Use `fd' to list files in DIR."
  (let* ((default-directory dir)
         (command "fd -H -t f -0")
         (output (shell-command-to-string command))
         (files-raw (split-string output "\0" t))
         (files (ale/minibuffer-append-metadata 'file files-raw))
         (file (completing-read (or prompt "Open file: ") files)))
    (find-file (concat dir file))))

(provide 'ale-minibuffer)
