;;; init.el --- -*- lexical-binding: t -*-

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'mailcap)
  (require 'files))
(defconst HAS-GUI (or (daemonp) (display-graphic-p)))
(defconst INIT-DIR (file-name-directory user-init-file))

(defcustom grandview-cache-dir (concat user-emacs-directory "grandview/")
  "Cache directory for grandview."
  :group 'grandview :type 'string)

(defcustom grandview-org-file (concat INIT-DIR "grandview.org")
  "Path for grandview main config .org file."
  :group 'grandview :type 'string)

(defcustom grandview-gc-cons-threshold 134217728 ; 128mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this.  If you experience
stuttering, increase this."
  :group 'grandview :type 'integer)

(defvar grandview--el-file (concat grandview-cache-dir "grandview.el"))
(defvar grandview--autoload-file (concat grandview-cache-dir "autoload.el"))
(defvar grandview--autoload-dir (concat grandview-cache-dir "autoloads/"))
(defvar grandview-debug-p nil "Show debug info of emacs-grandview.")

;; Keymaps
;; `grandview-files-map': Open files/dirs or operate on files
;; `grandview-mct-map':   'mct' is the acronym for "Minibuffer and Completions in Tandem".
;; `grandview-prog-map':  Programming related commands
;; `grandview-org-map':   Shortcuts for org related commands
;; `grandview-apps-map':  Useful utils such as format buffer, set frame opacity, etc.
(define-prefix-command 'grandview-files-map)
(define-prefix-command 'grandview-mct-map)
(define-prefix-command 'grandview-prog-map)
(define-prefix-command 'grandview-org-map)
(define-prefix-command 'grandview-apps-map)
(defalias 'grandview-tab-map tab-prefix-map)
(defalias 'grandview-reg-map ctl-x-r-map)
(if (boundp 'project-prefix-map)
    (defalias 'grandview-project-map project-prefix-map)
  (define-prefix-command 'grandview-project-map))

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

(defun silent! (fn &rest args)
  "Do not show any messages while executing FN and ARGS."
  (let ((inhibit-message t)) (apply fn args)))

(defmacro log! (&optional label &rest body)
  "Simple logging command.
Optional LABEL and BODY are evaluated and echoed out."
  (declare (indent defun))
  `(message "GrandView: %s %s" (or ,label "default") (or ,@body "Nothing")))

(defmacro setq! (&rest settings)
  "A stripped-down `customize-set-variable' with the syntax of `setq'.

This can be used as a drop-in replacement for `setq',
particularly when you know a variable has a custom setter (a :set
property in its `defcustom' declaration).  This triggers setters
with SETTINGS.  `setq' does not."
  (macroexp-progn
   (cl-loop for (var val) on settings by 'cddr
            collect `(funcall (or (get ',var 'custom-set) #'set) ',var ,val))))

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
      (push `(cons ,(pop body)
                   (let ((l ,(pop body))) (if (proper-list-p l) l (list l))))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(defun find-emacs-config ()
  "Edit `grandview-org-file'."
  (interactive)
  (find-file grandview-org-file))

(defun grandview--gen-tangle-path ()
  "Prepare metadata for `grandview-tangle'."
  (with-current-buffer (find-file-noselect grandview-org-file)
    (goto-char (point-min))
    (save-excursion
      (widen)
      (org-map-entries
       (lambda ()
         (org-with-point-at (point)
           (when (string= (org-get-heading) "Autoload")
             (let* ((package-name
                     (save-excursion
                       (org-up-heading-safe)
                       (let ((h (org-get-heading)))
                         (substring h (1+ (string-match "(\\(.*\\))" h)) (match-end 1)))))
                    (tangle-path (concat ":tangle \"" grandview--autoload-dir "+" package-name "\"")))
               (org-entry-put nil "header-args:emacs-lisp" tangle-path)))))))
    (save-buffer)
    (kill-this-buffer)))

(defun grandview--tangle (&optional force)
  "Tangle `grandview-org-file' when FORCE or its md5 changed."
  (let* ((md5-file (concat grandview-cache-dir "init.md5"))
         (old-md5 (when (file-exists-p md5-file) (read-file! md5-file)))
         (new-md5 (secure-hash 'md5 (read-file! grandview-org-file)))
         (org-confirm-babel-evaluate nil))
    (when (or force (not (string= old-md5 new-md5)))
      (when (file-exists-p grandview--el-file)
        (delete-file grandview--el-file))
      (with-temp-buffer
        (erase-buffer)
        (insert new-md5)
        (write-region (point-min) (point-max) md5-file))
      (require 'ob-tangle)
      (org-babel-tangle-file grandview-org-file grandview--el-file))))

(defun grandview--gen-autoload (&optional force)
  "Append `grandview--autoload-dir''s autoloads to `grandview--autoload-file'.
Only do it when FORCE or contents in `grandview--autoload-dir' changed."
  (require 'autoload)
  (let* ((autoload-md5 (concat grandview-cache-dir "autoload.md5"))
         (old-md5 (when (file-exists-p autoload-md5)
                    (read-file! autoload-md5)))
         (all-el-files (directory-files-recursively grandview--autoload-dir "\\.el$"))
         (files-as-str (with-temp-buffer
                         (dolist (file all-el-files)
                           (insert-file-contents file))
                         (buffer-string)))
         (new-md5 (secure-hash 'md5 files-as-str)))
    (when (or force (not (string= old-md5 new-md5)))
      (with-temp-file grandview--autoload-file
        (cl-loop with generated-autoload-file = nil
                 with inhibit-message = t
                 for file in all-el-files
                 for generated-autoload-load-name = (file-name-sans-extension file)
                 do (autoload-generate-file-autoloads file (current-buffer))))
      (with-temp-buffer
        (erase-buffer)
        (insert new-md5)
        (write-region (point-min) (point-max) autoload-md5)))))

(defun grandview-tangle (&optional force)
  "Tangle and generate autoloads for `grandview-org-file'.
FORCE is passed to `grandview--tangle' and
`grandview--gen-autoload'."
  (grandview--gen-tangle-path)
  (grandview--tangle force)
  (grandview--gen-autoload force))

(defun grandview--log (format &rest args)
  "Log to *Messages* if `grandview-debug-p' is on.
Does not display text in echo area, but still logs to
*Messages*.  FORMAT and ARGS are the same arguments as `message'."
  (when grandview-debug-p
    (let ((inhibit-message (active-minibuffer-window))
          (str (concat (propertize "GRANDVIEW " 'face 'font-lock-comment-face) format)))
      (apply 'message (push str args)))))

(defun grandview-profiler ()
  "Init info with packages loaded and init time."
  (when grandview-debug-p
    (let ((package-count 0)
          (time (emacs-init-time "%.3f"))
          (docstr "%d packages loaded in %ss"))
      (when (boundp 'straight--profile-cache)
        (setq package-count (+ (hash-table-size straight--profile-cache) package-count)))
      (run-with-timer 1 nil 'grandview--log docstr package-count time))))

(defvar use-package--deferred-pkgs-alist '(t))
(defun use-package-handler/:after-call (name _keyword hooks rest state)
  "Add keyword `:after-call' to `use-package'.
The purpose of this keyword is to expand the lazy-loading
capabilities of `use-package'.  Consult `use-package-concat' and
`use-package-process-keywords' for documentations of NAME, HOOKS,
REST and STATE."
  (if (plist-get state :demand)
      (use-package-process-keywords name rest state)
    (let ((fn (make-symbol (format "grandview--after-call-%s-h" name))))
      (use-package-concat
       `((fset ',fn
               (lambda (&rest _)
                 (grandview--log "Loading deferred package %s from %s" ',name ',fn)
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
  ;; Init package manager `straight.el' and package loader `use-package.el'
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
  ;; Hooks
  (if (boundp 'after-focus-change-function)
      (add-function :after after-focus-change-function
                    (lambda () (unless (frame-focus-state) (garbage-collect))))
    (add-hook 'after-focus-change-function 'garbage-collect))
  (add-hook 'emacs-startup-hook #'grandview-profiler)
  (add-hook 'kill-emacs-hook #'grandview-tangle -90)
  (with-eval-after-load 'org (add-hook 'org-tab-first-hook 'org-end-of-line))
  ;; Tangling
  (unless (file-exists-p grandview-cache-dir)
    (make-directory grandview--autoload-dir t)
    (grandview-tangle t))
  (load grandview--autoload-file nil t)
  (when (file-exists-p user-conf) (load user-conf nil t))
  (load grandview--el-file nil t)
  (setq gc-cons-threshold grandview-gc-cons-threshold))
