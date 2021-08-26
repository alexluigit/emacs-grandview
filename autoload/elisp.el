;;; core/autoload/elisp.el --- -*- lexical-binding: t -*-

(eval-when-compile (require 'subr-x))

;;;###autoload
(defun ale-log (format-string &rest args)
  "Log to *Messages* if `ale-debug-p' is on.
Does not display text in echo area, but still logs to *Messages*. Accepts the
same arguments as `message'."
  (when ale-debug-p
    (let ((inhibit-message (active-minibuffer-window))
          (str (concat (propertize "ALE " 'face 'font-lock-comment-face)
                       format-string)))
      (apply 'message (push str args)))))

;; Copied from `f.el'
(defun ale-f--read-bytes (path)
  "Read binary data from PATH.

Return the binary data as unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (insert-file-contents-literally path)
    (buffer-substring-no-properties (point-min) (point-max))))

;;;###autoload
(defun ale-f-read (path &optional coding)
  "Read text with PATH, using CODING.

CODING defaults to `utf-8'.
Return the decoded text as multibyte string."
  (decode-coding-string (ale-f--read-bytes path) (or coding 'utf-8)))

;;;###autoload
(defun ale-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

;;;###autoload
(defun ale-show-messages ()
  "Show *Messages* buffer."
  (interactive)
  (if-let ((win (get-buffer-window "*Messages*")))
      (delete-window win)
    (display-buffer-in-side-window
     (get-buffer "*Messages*")
     '((side . right)
       (window-width . 0.5)))))

;;;###autoload
(defun ale-erase-messages ()
  (interactive)
  (let ((inhibit-read-only t))
    (with-current-buffer "*Messages*" (erase-buffer))))

;;;###autoload
(defun dir! ()
  "Returns the directory of the emacs lisp file this macro is called from."
  (when-let (path (file!))
    (directory-file-name (file-name-directory path))))

;;;###autoload
(defun file! ()
  "Return the emacs lisp file this macro is called from."
  (cond ((bound-and-true-p byte-compile-current-file))
        (load-file-name)
        ((stringp (car-safe current-load-list))
         (car current-load-list))
        (buffer-file-name)
        ((error "Cannot get this file-path"))))

;;;###autoload
(defmacro letenv! (envvars &rest body)
  "Lexically bind ENVVARS in BODY, like `let' but for `process-environment'."
  (declare (indent 1))
  `(let ((process-environment (copy-sequence process-environment)))
     (dolist (var (list ,@(cl-loop for (var val) in envvars
                                   collect `(cons ,var ,val))))
       (setenv (car var) (cdr var)))
     ,@body))

;;; Mutation
;;;###autoload
(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))

;;;###autoload
(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.

If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  `(setq ,list
         (delq ,(if fetcher
                    `(funcall ,fetcher ,elt ,list)
                  elt)
               ,list)))

;;;###autoload
(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

;;;###autoload
(defmacro prependq! (sym &rest lists)
  "Prepend LISTS to SYM in place."
  `(setq ,sym (append ,@lists ,sym)))
