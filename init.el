;;; init.el --- -*- lexical-binding: t -*-

(defcustom grandview-cache-dir "/tmp/grandview/"
  "Cache directory for grandview."
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
    ('au-el (concat grandview-cache-dir "autoload.el"))
    ('au-dir (concat grandview-cache-dir "autoloads/"))
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
             (let* ((package-name
                     (save-excursion
                       (org-up-heading-safe)
                       (let ((h (org-get-heading)))
                         (substring h (1+ (string-match "(\\(.*\\))" h))
                                    (match-end 1)))))
                    (tangle-path (concat ":tangle \""
                                         (grandview--init-path 'au-dir)
                                         "+" package-name "\"")))
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
      (cl-loop for lib in (directory-files-recursively (grandview--init-path 'au-dir) "\\.el$")
               do (with-current-buffer (find-file-noselect lib)
                    (goto-char (point-min))
                    (insert ";;; -*- lexical-binding: t -*-\n\n")
                    (save-buffer)
                    (kill-this-buffer))))))

(defun grandview--gen-autoload (&optional force)
  "Generate autoload files for Grandview.
Only do it when FORCE or contents in autoload directory changed."
  (require 'autoload)
  (let* ((autoload-md5 (concat grandview-cache-dir "autoload.md5"))
         (old-md5 (when (file-exists-p autoload-md5)
                    (grandview--readfile autoload-md5)))
         (all-el-files (directory-files-recursively (grandview--init-path 'au-dir) "\\.el$"))
         (files-as-str (with-temp-buffer
                         (dolist (file all-el-files)
                           (insert-file-contents file))
                         (buffer-string)))
         (new-md5 (secure-hash 'md5 files-as-str)))
    (when (or force (not (string= old-md5 new-md5)))
      (with-temp-file (grandview--init-path 'au-el)
        (cl-loop with generated-autoload-file = nil
                 with inhibit-message = t
                 for file in all-el-files
                 for generated-autoload-load-name = (file-name-sans-extension file)
                 do (autoload-generate-file-autoloads file (current-buffer))))
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

(defun grandview-profiler ()
  "Profile init time."
  (let ((packages 0) (time (emacs-init-time "%.3f"))
        (docstr "%d packages loaded in %ss"))
    (when (boundp 'straight--profile-cache)
      (setq packages (+ (hash-table-size straight--profile-cache) packages)))
    (run-with-timer 1 nil 'grandview--log "GrandView Profiler"
                    (format docstr packages time))))

(let ((bootstrap (locate-user-emacs-file "straight/repos/straight.el/bootstrap.el"))
      (script "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el")
      (debug (or (getenv-internal "DEBUG") init-file-debug))
      (file-name-handler-alist nil))
  ;; Init package manager `straight.el'
  (setq straight-vc-git-default-clone-depth 1
        straight-check-for-modifications '(check-on-save find-when-checking)
        straight-repository-branch "develop")
  (unless (file-exists-p bootstrap)
    (with-current-buffer (url-retrieve-synchronously script 'silent 'inhibit-cookies)
      (goto-char (point-max)) (eval-print-last-sexp)))
  (load bootstrap nil 'nomessage)
  (straight-use-package 'bind-key) ; for `bind-keys' macro
  (straight-use-package '(once :type git :host github :repo "emacs-magus/once"))
  ;; Use inbuilt version of transient
  (straight-use-package `(transient ,@(when (> emacs-major-version 27) '(:type built-in))))
  ;; Load user config
  (when (file-exists-p (grandview--init-path 'user))
    (load (grandview--init-path 'user) (not debug) t))
  ;; Tangle and load Grandview
  ;; "Initiate spin!" -- Joseph Cooper
  (unless (file-exists-p grandview-cache-dir)
    (make-directory (grandview--init-path 'au-dir) t)
    (grandview-tangle t))
  (load (grandview--init-path 'au-el) (not debug) t)
  (load (grandview--init-path 'main) (not debug) t)
  (add-hook 'kill-emacs-hook #'grandview-tangle -90)
  (add-hook 'after-init-hook
            (lambda ()
              (require 'server)
              (unless (server-running-p)
                (server-start))))
  ;; Setup PATH on macOS
  (when (memq window-system '(mac ns))
    (straight-use-package 'exec-path-from-shell)
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "DOTPATH")
    (exec-path-from-shell-copy-env "XDG_DATA_HOME"))
  ;; Show profiler when debugging
  (when debug (grandview-profiler))
  ;; Setup garbage collection
  (add-function :after after-focus-change-function
                (lambda () (unless (frame-focus-state) (garbage-collect))))
  (setq gc-cons-threshold grandview-gc-cons-threshold))
