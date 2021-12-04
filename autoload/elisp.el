;;; autoload/elisp.el --- -*- lexical-binding: t -*-

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
(defun ale-show-messages (&optional erase)
  "Show *Messages* buffer."
  (interactive "P")
  (when erase
    (let ((inhibit-read-only t))
      (with-current-buffer "*Messages*" (erase-buffer))))
  (if-let ((win (get-buffer-window "*Messages*")))
      (delete-window win)
    (display-buffer-in-side-window
     (get-buffer "*Messages*")
     '((side . right)
       (window-width . 0.5)))))

;;;###autoload
(defadvice find-library (around always-follow-link activate)
  "Always follow symlink when using `find-library'.

Package managers like `straight.el' use symlink to manage
package/libraries. This advice will enable user always find
libraries's truename."
  (let ((vc-follow-symlinks t)) ad-do-it))

;;;###autoload
(defun silent! (fn &rest args)
  "Do not show any messages while executing FN. Used as an
advisor."
  (let ((inhibit-message t)) (apply fn args)))

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

;;; Closure factories

;;;###autoload
(defmacro letf! (bindings &rest body)
  "Temporarily rebind function, macros, and advice in BODY.
Intended as syntax sugar for `cl-letf', `cl-labels', `cl-macrolet', and
temporary advice.
BINDINGS is either:
  A list of, or a single, `defun', `defun*', or `defmacro' forms.
  A list of (PLACE VALUE) bindings as `cl-letf*' would accept.
TYPE is one of:
  `defun' (uses `cl-letf')
  `defun*' (uses `cl-labels'; allows recursive references),
  `defmacro' (uses `cl-macrolet')
\(fn ((TYPE NAME ARGLIST &rest BODY) ...) BODY...)"
  (declare (indent defun))
  (setq body (macroexp-progn body))
  (when (memq (car bindings) '(defun defun* defmacro))
    (setq bindings (list bindings)))
  (dolist (binding (reverse bindings) body)
    (let ((type (car binding))
          (rest (cdr binding)))
      (setq
       body (pcase type
              (`defmacro `(cl-macrolet ((,@rest)) ,body))
              ((or `defun `defun*)
               `(cl-letf ((,(car rest) (symbol-function #',(car rest))))
                  (ignore ,(car rest))
                  ,(if (eq type 'defun*)
                       `(cl-labels ((,@rest)) ,body)
                     `(cl-letf (((symbol-function #',(car rest))
                                 (fn! ,(cadr rest) ,@(cddr rest))))
                        ,body))))
              (_
               (when (eq (car-safe type) 'function)
                 (setq type (list 'symbol-function type)))
               (list 'cl-letf (list (cons type rest)) body)))))))

;;;###autoload
(defmacro fn! (arglist &rest body)
  "Returns (cl-function (lambda ARGLIST BODY...))
The closure is wrapped in `cl-function', meaning ARGLIST will accept anything
`cl-defun' will. Implicitly adds `&allow-other-keys' if `&key' is present in
ARGLIST."
  (declare (indent defun) (doc-string 1) (pure t) (side-effect-free t))
  `(cl-function
    (lambda
      ,(letf! (defun* allow-other-keys (args)
                (mapcar
                 (lambda (arg)
                   (cond ((nlistp (cdr-safe arg)) arg)
                         ((listp arg) (allow-other-keys arg))
                         (arg)))
                 (if (and (memq '&key args)
                          (not (memq '&allow-other-keys args)))
                     (if (memq '&aux args)
                         (let (newargs arg)
                           (while args
                             (setq arg (pop args))
                             (when (eq arg '&aux)
                               (push '&allow-other-keys newargs))
                             (push arg newargs))
                           (nreverse newargs))
                       (append args (list '&allow-other-keys)))
                   args)))
         (allow-other-keys arglist))
      ,@body)))

;;; Mutation

;;;###autoload
(defmacro setq! (&rest settings)
  "A stripped-down `customize-set-variable' with the syntax of `setq'.

This can be used as a drop-in replacement for `setq'. Particularly when you know
a variable has a custom setter (a :set property in its `defcustom' declaration).
This triggers setters. `setq' does not."
  (macroexp-progn
   (cl-loop for (var val) on settings by 'cddr
            collect `(funcall (or (get ',var 'custom-set) #'set)
                              ',var ,val))))

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
