;;; autoload/minibuffer.el --- -*- lexical-binding: t -*-

(defgroup ale-minibuffer ()
  "Extensions for the minibuffer."
  :group 'minibuffer)

;;;###autoload
(defun ale-minibuffer-append-metadata (meta completions)
  "doc"
  (let ((entry (if (functionp meta)
                   `(metadata (annotation-function . ,meta))
                 `(metadata (category . ,meta)))))
    (lambda (string pred action)
      (if (eq action 'metadata)
          entry
        (complete-with-action action completions string pred)))))

(defun ale-minibuffer-completing-read (prompt collection &rest comp-read-args)
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

;;;###autoload
(defun ale-minibuffer--files-in-directory (dir &optional prompt)
  "Use `fd' to list files in DIR."
  (let* ((default-directory dir)
         (command "fd -H -t f -0")
         (output (shell-command-to-string command))
         (files-raw (split-string output "\0" t))
         (files (ale-minibuffer-append-metadata 'file files-raw))
         (file (completing-read (or prompt "Open file: ") files)))
    (find-file (concat dir "/" file))))

