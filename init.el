;;; init.el --- -*- lexical-binding: t -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'mailcap)
  (require 'files))

(defconst IS-GUI (or (daemonp) (display-graphic-p)))
(defconst INIT-DIR (file-name-directory user-init-file))
(defvar ale-debug-p nil)
(defvar ale-cache-dir (concat user-emacs-directory "ale/"))
(defvar ale-full-config-org (concat INIT-DIR "ale.org"))
(defvar ale-full-config (concat ale-cache-dir "full.el"))
(defun ale-minimal-config () (concat ale-cache-dir "minimal.el"))
(defvar ale-autoload-file (concat ale-cache-dir "autoload.el"))
(defvar ale-autoload-dir (concat ale-cache-dir "autoload/"))
(defvar ale-gc-cons-threshold 134217728 ; 128mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this.  If you experience
stuttering, increase this.")

;; `ale-files-map': Open files/dirs or operate on files
;; `ale-mct-map':   'mct' is the acronym for "Minibuffer and Completions in Tandem".
;; `ale-prog-map':  Programming related commands
;; `ale-org-map':   Shortcuts for org related commands
;; `ale-apps-map':  Useful utils such as format buffer, set frame opacity, etc.
(define-prefix-command 'ale-files-map)
(define-prefix-command 'ale-mct-map)
(define-prefix-command 'ale-prog-map)
(define-prefix-command 'ale-org-map)
(define-prefix-command 'ale-apps-map)
(defalias 'tab-map tab-prefix-map)
(defalias 'register-map ctl-x-r-map)
(if (boundp 'project-prefix-map)
    (defalias 'project-map project-prefix-map)
  (define-prefix-command 'project-prefix-map))

(defun read-file! (path &optional coding)
  "Read text with PATH, using CODING.
CODING defaults to `utf-8'.
Return the decoded text as multibyte string."
  (let ((str (with-temp-buffer
               (set-buffer-multibyte nil)
               (setq buffer-file-coding-system 'binary)
               (insert-file-contents-literally path)
               (buffer-substring-no-properties (point-min) (point-max)))))
  (decode-coding-string str (or coding 'utf-8))))

(defun listify! (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (proper-list-p exp) exp (list exp)))

(defun silent! (fn &rest args)
  "Do not show any messages while executing FN and ARGS."
  (let ((inhibit-message t)) (apply fn args)))

(defmacro log! (&optional label &rest body)
  "Simple logging command.
Optional LABEL and BODY are evaluated and echoed out."
  (declare (indent defun))
  `(message "ALE: %s %s" (or ,label "default") (or ,@body "Nothing")))

(defmacro setq! (&rest settings)
  "A stripped-down `customize-set-variable' with the syntax of `setq'.

This can be used as a drop-in replacement for `setq',
particularly when you know a variable has a custom setter (a :set
property in its `defcustom' declaration).  This triggers setters
with SETTINGS.  `setq' does not."
  (macroexp-progn
   (cl-loop for (var val) on settings by 'cddr
            collect `(funcall (or (get ',var 'custom-set) #'set)
                              ',var ,val))))

(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))

(defmacro delq! (elt list)
  "`delq' ELT from LIST in-place."
  `(setq ,list (delq ,elt ,list)))

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

(defmacro prependq! (sym &rest lists)
  "Prepend LISTS to SYM in place."
  `(setq ,sym (append ,@lists ,sym)))

(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.
ARGLIST is as in `defun'.  WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.
\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (listify! ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(defmacro undefadvice! (symbol _arglist &optional docstring &rest body)
  "Undefine an advice called SYMBOL.
This has the same signature as `defadvice!' an exists as an easy undefiner when
testing advice (when combined with `rotate-text').
\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (let (where-alist)
    (unless (stringp docstring)
      (push docstring body))
    (while (keywordp (car body))
      (push `(cons ,(pop body) (listify! ,(pop body)))
            where-alist))
    `(dolist (targets (list ,@(nreverse where-alist)))
       (dolist (target (cdr targets))
         (advice-remove target #',symbol)))))

(defun find-emacs-config ()
  "Editing emacs init file."
  (interactive)
  (find-file ale-full-config-org))

(defun ale-tangle--all (&optional force)
  "Tangle `ale-full-config-org' when FORCE or its md5 changed."
  (let* ((md5-file (concat ale-cache-dir "init.md5"))
         (old-md5 (when (file-exists-p md5-file)
                    (read-file! md5-file)))
         (new-md5 (secure-hash 'md5 (read-file! ale-full-config-org))))
    (when (or force (not (string= old-md5 new-md5)))
      (when (file-exists-p (ale-minimal-config))
        (delete-file (ale-minimal-config)))
      (when (file-exists-p ale-full-config)
        (delete-file ale-full-config))
      (with-temp-buffer
        (erase-buffer)
        (insert new-md5)
        (write-region (point-min) (point-max) md5-file))
      (require 'ob-tangle)
      (org-babel-tangle-file ale-full-config-org ale-full-config))))

(defun ale-tangle--gen-autoload (&optional force)
  "Append `ale-autoload-dir''s autoloads to `ale-autoload-file'.
Only do it when FORCE or contents in `ale-autoload-dir' changed."
  (require 'autoload)
  (let* ((autoload-md5 (concat ale-cache-dir "autoload.md5"))
         (old-md5 (when (file-exists-p autoload-md5)
                    (read-file! autoload-md5)))
         (all-el-files (directory-files-recursively ale-autoload-dir "\\.el$"))
         (files-as-str (with-temp-buffer
                         (dolist (file all-el-files)
                           (insert-file-contents file))
                         (buffer-string)))
         (new-md5 (secure-hash 'md5 files-as-str)))
    (when (or force (not (string= old-md5 new-md5)))
      (with-temp-file ale-autoload-file
        (cl-loop with generated-autoload-file = nil
                 with inhibit-message = t
                 for file in all-el-files
                 for generated-autoload-load-name = (file-name-sans-extension file)
                 do (autoload-generate-file-autoloads file (current-buffer))))
      (with-temp-buffer
        (erase-buffer)
        (insert new-md5)
        (write-region (point-min) (point-max) autoload-md5)))))

(defun ale-tangle--prepare-heading ()
  "Prepare metadata for `ale-tangle'."
  (with-current-buffer (find-file-noselect ale-full-config-org)
    (save-excursion
      (widen)
      (org-map-entries
       (lambda ()
         (org-with-point-at (point-min)
           (when (string= (org-get-heading) "Autoload")
             (let* ((package-name
                     (save-excursion
                       (org-up-heading-safe)
                       (let ((h (org-get-heading)))
                         (substring h (1+ (string-match "(\\(.*\\))" h)) (match-end 1)))))
                    (tangle-path (concat ":tangle \"" ale-autoload-dir package-name "\"")))
               (org-entry-put (point) "header-args:emacs-lisp" tangle-path)))))))
    (save-buffer)
    (kill-this-buffer)))

(defun ale-tangle (&optional force)
  "Tangle and generate autoloads for `ale-full-config-org'.
FORCE is passed to `ale-tangle--all' and
`ale-tangle--gen-autoload'."
  (ale-tangle--prepare-heading)
  (ale-tangle--all force)
  (ale-tangle--gen-autoload force))

(defun ale-debug-log (format &rest args)
  "Log to *Messages* if `ale-debug-p' is on.
Does not display text in echo area, but still logs to
*Messages*.  FORMAT and ARGS are the same arguments as `message'."
  (when ale-debug-p
    (let ((inhibit-message (active-minibuffer-window))
          (str (concat (propertize "ALE " 'face 'font-lock-comment-face) format)))
      (apply 'message (push str args)))))

(defun ale-debug-profiler ()
  "Init info with packages loaded and init time."
  (when ale-debug-p
    (let ((package-count 0)
          (time (emacs-init-time "%.3f"))
          (docstr "%d packages loaded in %ss"))
      (when (boundp 'straight--profile-cache)
        (setq package-count (+ (hash-table-size straight--profile-cache) package-count)))
      (run-with-timer 1 nil 'ale-debug-log docstr package-count time))))

(defvar use-package--deferred-pkgs-alist '(t))
(defun use-package-handler/:after-call (name _keyword hooks rest state)
  "Add keyword `:after-call' to `use-package'.
The purpose of this keyword is to expand the lazy-loading
capabilities of `use-package'.  Consult `use-package-concat' and
`use-package-process-keywords' for documentations of NAME, HOOKS,
REST and STATE."
  (if (plist-get state :demand)
      (use-package-process-keywords name rest state)
    (let ((fn (make-symbol (format "ale--after-call-%s-h" name))))
      (use-package-concat
       `((fset ',fn
               (lambda (&rest _)
                 (ale-debug-log "Loading deferred package %s from %s" ',name ',fn)
                 (condition-case e
                     (let ((default-directory user-emacs-directory))
                       (require ',name))
                   ((debug error)
                    (message "Failed to load deferred package %s: %s" ',name e)))
                 (when-let (deferral-list (assq ',name use-package--deferred-pkgs-alist))
                   (dolist (hook (cdr deferral-list))
                     (advice-remove hook #',fn)
                     (remove-hook hook #',fn))
                   (setq use-package--deferred-pkgs-alist
                         (delq deferral-list use-package--deferred-pkgs-alist))
                   (unintern ',fn nil)))))
       (let (forms)
         (dolist (hook hooks forms)
           (push (if (string-match-p "-\\(?:functions\\|hook\\)$" (symbol-name hook))
                     `(add-hook ',hook #',fn)
                   `(advice-add #',hook :before #',fn))
                 forms)))
       `((unless (assq ',name use-package--deferred-pkgs-alist)
           (push '(,name) use-package--deferred-pkgs-alist))
         (nconc (assq ',name use-package--deferred-pkgs-alist)
                '(,@hooks)))
       (use-package-process-keywords name rest state)))))

(defadvice! +org-toggle-comment (fn &rest args)
  "Enhanced drop-in replacement for `org-toggle-comment'."
  :around #'org-toggle-comment
  (if (region-active-p)
      (progn
        (exchange-point-and-mark)
        (let ((end (region-end)) last-point)
          (while (< (point) end)
            (setq last-point (point))
            (apply fn args)
            (org-forward-heading-same-level 1)
            (when (eq last-point (point))
              (org-forward-element)))))
    (apply fn args)))

(let* ((init-dir (file-name-directory user-init-file))
       (user-conf (concat init-dir "user.el"))
       (bootstrap (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
       (script "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el")
       (file-name-handler-alist nil))
  (setq straight-use-package-by-default t)
  (setq straight-vc-git-default-clone-depth 1)
  (setq straight-check-for-modifications '(check-on-save find-when-checking))
  (setq straight-repository-branch "develop")
  (setq use-package-always-defer t)
  (unless (file-exists-p bootstrap)
    (with-current-buffer (url-retrieve-synchronously script 'silent 'inhibit-cookies)
      (goto-char (point-max)) (eval-print-last-sexp)))
  (load bootstrap nil 'nomessage)
  (straight-use-package 'use-package)
  (require 'use-package-core)
  (push :after-call use-package-deferring-keywords)
  (setq use-package-keywords (use-package-list-insert :after-call use-package-keywords :after))
  (defalias 'use-package-normalize/:after-call #'use-package-normalize-symlist)
  (if (boundp 'after-focus-change-function)
      (add-function :after after-focus-change-function
                    (lambda () (unless (frame-focus-state) (garbage-collect))))
    (add-hook 'after-focus-change-function 'garbage-collect))
  (add-hook 'emacs-startup-hook #'ale-debug-profiler)
  (add-hook 'kill-emacs-hook #'ale-tangle -90)
  (with-eval-after-load 'org (add-hook 'org-tab-first-hook 'org-end-of-line))
  (unless (file-exists-p ale-cache-dir)
    (make-directory ale-cache-dir t)
    (ale-tangle t))
  (load ale-autoload-file nil t)
  (when (file-exists-p user-conf) (load user-conf nil t))
  (load (ale-minimal-config) nil t)
  (when IS-GUI (load ale-full-config nil t))
  (setq gc-cons-threshold ale-gc-cons-threshold))
