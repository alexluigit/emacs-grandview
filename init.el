;;; init.el --- -*- lexical-binding: t -*-

(defcustom grandview-home "~/.config/emacs/"
  "Path to place the repository of grandview."
  :group 'grandview :type 'string)

(defcustom grandview-envs
  (let ((config-home (expand-file-name "~/.config"))
        (data-home (expand-file-name "~/.local/share")))
    `(("XDG_CONFIG_HOME" . ,config-home)
      ("XDG_DATA_HOME" . ,data-home)
      ("GNUPGHOME" . ,(expand-file-name "gnupg" data-home))
      ("PATH" . ,(string-join
                  (list (expand-file-name "python/bin" data-home)
                        "/opt/homebrew/bin" "/opt/homebrew/sbin"
                        "/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin"
                        (expand-file-name "~/.local/bin")
                        (expand-file-name "cargo/bin" data-home)
                        (expand-file-name "go/bin" data-home)) ":"))))
  "Use these environment variables in GUI emacs."
  :group 'grandview :type 'list)

(defcustom grandview-cache-dir "/tmp/grandview/"
  "Cache directory for grandview.
This path is added to your `load-path'."
  :group 'grandview :type 'string)

(defcustom grandview-org-file (concat (file-name-directory user-init-file) "grandview.org")
  "Path for grandview main config .org file."
  :group 'grandview :type 'string)

(defvar grandview-theme 'modus-vivendi)
(defvar grandview-font-size 140)
(defvar grandview-default-font "Iosevka Nerd Font Mono")
(defvar grandview-fixed-font "Sarasa Mono SC")
(defvar grandview-variable-font "Sarasa Mono SC")
(defvar grandview-CJK-font "LXGW WenKai Mono")

(defcustom grandview-gc-cons-threshold 134217728 ; 128mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this.  If you experience
stuttering, increase this."
  :group 'grandview :type 'integer)

;; Keymaps
;; `grandview-files-map':    Open files/dirs or operations on files
;; `grandview-mct-map':      'mct' is the acronym for "Minibuffer and Completions in Tandem"
;; `grandview-prog-map':     Programming related commands
;; `grandview-org-map':      Shortcuts for org related commands
;; `grandview-win/tabs-map': Commands related to windows/workspaces
;; `grandview-apps-map':     Useful utils such as format buffer, set frame opacity, etc.
;; `grandview-reg/rect-map': Keymap for subcommands of \\`C-x r'.
;; `grandview-project-map':  Project commands.
(define-prefix-command 'grandview-files-map)
(define-prefix-command 'grandview-mct-map)
(define-prefix-command 'grandview-prog-map)
(define-prefix-command 'grandview-org-map)
(define-prefix-command 'grandview-apps-map)
(define-prefix-command 'grandview-win/tabs-map)
(define-prefix-command 'grandview-reg-map)
(define-prefix-command 'grandview-project-map)
(defalias 'grandview-win/tabs-map tab-prefix-map)
(defalias 'grandview-reg-map ctl-x-r-map)
(when (boundp 'project-prefix-map)
  (defalias 'grandview-project-map project-prefix-map))

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

(defadvice! org-toggle-comment-ad (fn &rest args)
  "Drop-in replacement for `org-toggle-comment'.
This allows `org-toggle-comment' to toggle comment for all the
entries with the same level in the active region while behaves
the same when the region is inactive.  This is useful for
debugging code blocks in a org config file."
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

(defun grandview--readfile (path)
  "Return the decoded text in PATH as multibyte string."
  (let ((str (with-temp-buffer
               (set-buffer-multibyte nil)
               (setq buffer-file-coding-system 'binary)
               (insert-file-contents-literally path)
               (buffer-substring-no-properties (point-min) (point-max)))))
  (decode-coding-string str 'utf-8)))

(defun grandview--log (&optional label string)
  "Simple logging command.
Optional LABEL and STRING are echoed out."
  (let* ((label-str (cond ((symbolp label) (symbol-name label))
                          ((stringp label) label)
                          (t "GRANDVIEW")))
         (label (propertize label-str 'face 'font-lock-builtin-face)))
    (prog1 nil (message "%s" (format "%s: %s" label string)))))

(defun grandview--init-path (type)
  "Get grandview's init path according to TYPE."
  (pcase type
    ('main (concat grandview-cache-dir "grandview.el"))
    ('def-el (concat grandview-cache-dir "grandview-loaddefs.el"))
    ('def-dir (concat grandview-cache-dir "autoloads/"))
    ('user (concat (file-name-directory grandview-org-file) "user.el"))))

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
             (let* ((title (save-excursion (org-up-heading-safe) (org-get-heading)))
                    (memo (ignore-errors
                            (substring title (1+ (string-match "(\\(.*\\))" title))
                                       (match-end 1))))
                    (name (or memo (replace-regexp-in-string (regexp-quote " ") "_" title t t)))
                    (.el? (ignore-errors (string= ".el" (substring name -3))))
                    (tangle-path (concat ":tangle \""
                                         (grandview--init-path 'def-dir)
                                         "+" name (if .el? "" ".el") "\"")))
               (org-entry-put nil "header-args:emacs-lisp" tangle-path)))))))
    (save-buffer)
    (kill-this-buffer)))

(defun grandview--tangle (&optional force)
  "Tangle `grandview-org-file' when FORCE or its md5 changed."
  (let* ((md5-file (concat grandview-cache-dir "init.md5"))
         (old-md5 (when (file-exists-p md5-file) (grandview--readfile md5-file)))
         (new-md5 (secure-hash 'md5 (grandview--readfile grandview-org-file)))
         org-confirm-babel-evaluate find-file-hook
         kill-buffer-hook write-file-functions)
    (when (or force (not (string= old-md5 new-md5)))
      (when (file-exists-p (grandview--init-path 'main))
        (delete-file (grandview--init-path 'main)))
      (with-temp-buffer
        (erase-buffer)
        (insert new-md5)
        (write-region (point-min) (point-max) md5-file))
      (require 'ob-tangle)
      (org-babel-tangle-file grandview-org-file (grandview--init-path 'main))
      (cl-loop for lib in (directory-files-recursively (grandview--init-path 'def-dir) "\\.el$")
               do (with-temp-buffer
                    (insert ";;; -*- lexical-binding: t -*-\n\n")
                    (insert-file-contents lib)
                    (write-region nil nil lib))))))

(defun grandview--gen-autoload (&optional force)
  "Generate autoload files for Grandview.
Only do it when FORCE or contents in autoload directory changed."
  (let* ((autoload-md5 (concat grandview-cache-dir "autoload.md5"))
         (old-md5 (when (file-exists-p autoload-md5)
                    (grandview--readfile autoload-md5)))
         (def-dir (grandview--init-path 'def-dir))
         (all-el-files (directory-files-recursively def-dir "\\.el$"))
         (files-as-str (with-temp-buffer
                         (dolist (file all-el-files)
                           (insert-file-contents file))
                         (buffer-string)))
         (new-md5 (secure-hash 'md5 files-as-str)))
    (when (or force (not (string= old-md5 new-md5)))
      (let ((generate-autoload-file nil) (inhibit-message t))
        (loaddefs-generate def-dir (grandview--init-path 'def-el)))
      (with-temp-buffer
        (erase-buffer)
        (insert new-md5)
        (write-region (point-min) (point-max) autoload-md5)))))

(defun grandview-config ()
  "Edit `grandview-org-file'."
  (interactive)
  (find-file grandview-org-file))

(defun grandview-tangle (&optional force)
  "Tangle and generate autoloads for `grandview-org-file'.
When FORCE, ensure the tangle process and autoloads generation."
  (grandview--gen-tangle-path)
  (grandview--tangle force)
  (grandview--gen-autoload force))

(let ((debug (or (getenv-internal "DEBUG") init-file-debug))
      file-name-handler-alist)
  (use-package bind-key :ensure t) ; for `bind-keys' macro
  (use-package once :vc (:url "https://github.com/emacs-magus/once"))
  ;; Load user config
  (when (file-exists-p (grandview--init-path 'user))
    (load (grandview--init-path 'user) (not debug) t))
  ;; Tangle and load Grandview
  ;; "Initiate spin!" -- Joseph Cooper
  (add-to-list 'load-path grandview-cache-dir)
  (unless (file-exists-p grandview-cache-dir)
    (make-directory (grandview--init-path 'def-dir) t)
    (grandview-tangle t))
  (add-hook 'kill-emacs-hook #'grandview-tangle -90)
  (add-hook 'after-init-hook (lambda ()
                               (require 'server)
                               (unless (server-running-p) (server-start))
                               (select-frame-set-input-focus (selected-frame))))
  ;; Setup PATH
  (pcase-dolist (`(,name . ,value) grandview-envs)
    (setenv name value)
    (when (string-equal "PATH" name)
      (setq exec-path (append (parse-colon-path value) (list exec-directory)))
      (setq-default eshell-path-env value)))
  (require 'grandview-loaddefs nil (not debug))
  (require 'grandview nil (not debug))
  ;; Setup garbage collection
  (add-function :after after-focus-change-function
                (lambda () (unless (frame-focus-state) (garbage-collect))))
  (setq gc-cons-threshold grandview-gc-cons-threshold))
