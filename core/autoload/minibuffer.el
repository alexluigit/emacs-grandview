;;; core/autoload/minibuffer.el --- -*- lexical-binding: t -*-

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
    (find-file (concat dir file))))
(autoload 'consult-grep "consult")
(autoload 'consult-line "consult")
(autoload 'consult-imenu "consult-imenu")
(autoload 'consult-outline "consult")

(defun ale-orderless-pinyin-only-initialism (pattern)
  "Leading pinyin initialism regex generator."
  (ale-pinyin-build-regexp-string pattern t nil t))

;;;###autoload
(defun ale-orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher using the equals sign as a prefix."
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

;;;###autoload
(defun ale-orderless-initialism-dispatcher (pattern _index _total)
  "Leading initialism dispatcher using the comma sign as a prefix."
  (when (string-prefix-p "," pattern)
    `(orderless-strict-leading-initialism . ,(substring pattern 1))))

;;;###autoload
(defun ale-orderless-pinyin-dispatcher (pattern _index _total)
  "Pinyin initialism dispatcher using the backtick sign as a prefix."
  (when (string-prefix-p "`" pattern)
    `(ale-orderless-pinyin-only-initialism . ,(substring pattern 1))))

;;;###autoload
(defun ale-orderless-without-literal-dispatcher (pattern _index _total)
  (when (string-prefix-p "~" pattern)
    `(orderless-without-literal . ,(substring pattern 1))))

(defvar ale-embark-become-general-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'consult-grep)
    map)
  "General custom cross-package `embark-become' keymap.")

(defvar ale-embark-become-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") 'consult-line)
    (define-key map (kbd "i") 'consult-imenu)
    (define-key map (kbd "s") 'consult-outline) ; as my default is 'M-s s'
    map)
  "Line-specific custom cross-package `embark-become' keymap.")

(defvar embark-become-file+buffer-map)
(autoload 'project-switch-to-buffer "project")
(autoload 'project-find-file "project")

(defvar ale-embark-become-file+buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map embark-become-file+buffer-map)
    (define-key map (kbd "B") 'project-switch-to-buffer)
    (define-key map (kbd "F") 'project-find-file)
    map)
  "File+buffer custom cross-package `embark-become' keymap.")

(defvar embark-become-keymaps)

;;;###autoload
(define-minor-mode ale-embark-keymaps
  "Add or remove keymaps from Embark.
This is based on the value of `ale-embark-add-keymaps'
and is meant to keep things clean in case I ever wish to disable
those so-called 'extras'."
  :init-value nil
  :global t
  (let ((maps '(ale-embark-become-general-map
                ale-embark-become-line-map
                ale-embark-become-file+buffer-map)))
    (if ale-embark-keymaps
        (dolist (map maps)
          (cl-pushnew map embark-become-keymaps))
      (setq embark-become-keymaps
            (dolist (map maps)
              (delete map embark-become-keymaps))))))
