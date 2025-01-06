;;; init.el --- -*- lexical-binding: t -*-

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

(defcustom grandview-cache-dir
  (expand-file-name "grandview/" user-emacs-directory)
  "Cache directory for grandview.
This path is added to your `load-path'."
  :group 'grandview :type 'string)

(defvar grandview--dot-org
  (expand-file-name "grandview.org" (file-name-directory user-init-file)))

(defun grandview--readfile (path)
  "Return the decoded text in PATH as multibyte string."
  (let ((str (with-temp-buffer
               (set-buffer-multibyte nil)
               (setq buffer-file-coding-system 'binary)
               (insert-file-contents-literally path)
               (buffer-substring-no-properties (point-min) (point-max)))))
    (decode-coding-string str 'utf-8)))

(defun grandview--path (type)
  "Get grandview's init path according to TYPE."
  (pcase type
    ('main (expand-file-name "grandview.el" grandview-cache-dir))
    ('main-md5 (expand-file-name "grandview.el.md5" grandview-cache-dir))
    ('def-el (expand-file-name "grandview-loaddefs.el" grandview-cache-dir))
    ('def-dir (expand-file-name "autoloads/" grandview-cache-dir))
    ('def-md5 (expand-file-name "grandview-autoloads.md5" grandview-cache-dir))
    ('user (expand-file-name "user.el" (file-name-directory user-init-file)))))

(defun grandview--gen-tangle-path ()
  "Prepare metadata for `grandview-tangle'."
  (with-current-buffer (find-file-noselect grandview--dot-org)
    (goto-char (point-min))
    (save-excursion
      (widen)
      (org-map-entries
       (lambda ()
         (org-with-point-at (point)
           (when (string= (org-get-heading) "Autoload")
             (let* ((title (save-excursion
                             (org-up-heading-safe) (org-get-heading)))
                    (memo (ignore-errors
                            (substring
                             title (1+ (string-match "(\\(.*\\))" title))
                             (match-end 1))))
                    (name (or memo (replace-regexp-in-string
                                    (regexp-quote " ") "_" title t t)))
                    (.el? (ignore-errors (string= ".el" (substring name -3))))
                    (tangle-path (concat ":tangle \""
                                         (grandview--path 'def-dir)
                                         "+" name (if .el? "" ".el") "\"")))
               (org-entry-put nil "header-args:emacs-lisp" tangle-path)))))))
    (save-buffer)
    (kill-this-buffer)))

(defun grandview--tangle (&optional force)
  "Tangle grandview.org to grandview.el.
The tanglement only happens when the hashed md5 string changed after
last change or FORCE is non nil."
  (let* ((md5-file (grandview--path 'main-md5))
         (old-md5 (when (file-exists-p md5-file)
                    (grandview--readfile md5-file)))
         (new-md5 (secure-hash 'md5 (grandview--readfile
                                     grandview--dot-org)))
         org-confirm-babel-evaluate find-file-hook
         kill-buffer-hook write-file-functions)
    (when (or force (not (string= old-md5 new-md5)))
      (when (file-exists-p (grandview--path 'main))
        (delete-file (grandview--path 'main)))
      (with-temp-buffer
        (erase-buffer)
        (insert new-md5)
        (write-region (point-min) (point-max) md5-file))
      (require 'ob-tangle)
      (org-babel-tangle-file
       grandview--dot-org (grandview--path 'main))
      (cl-loop for lib in (directory-files-recursively
                           (grandview--path 'def-dir) "\\.el$")
               do (with-temp-buffer
                    (insert ";;; -*- lexical-binding: t -*-\n\n")
                    (insert-file-contents lib)
                    (write-region nil nil lib))))))

(defun grandview--gen-autoload (&optional force)
  "Generate autoload files for Grandview.
Only do it when FORCE or contents in autoload directory changed."
  (let* ((autoload-md5 (grandview--path 'def-md5))
         (old-md5 (when (file-exists-p autoload-md5)
                    (grandview--readfile autoload-md5)))
         (def-dir (grandview--path 'def-dir))
         (all-el-files (directory-files-recursively def-dir "\\.el$"))
         (files-as-str (with-temp-buffer
                         (dolist (file all-el-files)
                           (insert-file-contents file))
                         (buffer-string)))
         (new-md5 (secure-hash 'md5 files-as-str)))
    (when (or force (not (string= old-md5 new-md5)))
      (let ((generate-autoload-file nil) (inhibit-message t))
        (loaddefs-generate def-dir (grandview--path 'def-el)))
      (with-temp-buffer
        (erase-buffer)
        (insert new-md5)
        (write-region (point-min) (point-max) autoload-md5)))))

(defun grandview-tangle (&optional force)
  "Tangle and generate grandview's autoloads.
When FORCE, ensure the tangle process and autoloads generation."
  (grandview--gen-tangle-path)
  (grandview--tangle force)
  (grandview--gen-autoload force))

(let ((debug (or (getenv-internal "DEBUG") init-file-debug))
      file-name-handler-alist)
  (unless (file-exists-p grandview-cache-dir)
    (make-directory (grandview--path 'def-dir) t)
    (grandview-tangle t)) ; "Initiate spin!" -- Joseph Cooper
  (add-hook 'kill-emacs-hook #'grandview-tangle -90)
  (add-to-list 'load-path grandview-cache-dir)
  (require 'grandview-macros nil (not debug))
  (when (file-exists-p (grandview--path 'user))
    (load (grandview--path 'user) (not debug) t))
  (pcase-dolist (`(,name . ,value) grandview-envs)
    (setenv name value)
    (when (string-equal "PATH" name)
      (setq exec-path (append (parse-colon-path value)
                              (list exec-directory)))
      (setq-default eshell-path-env value)))
  (require 'grandview-loaddefs nil (not debug))
  (require 'grandview nil (not debug))
  (add-function :after after-focus-change-function
                (lambda () (unless (frame-focus-state)
                        (garbage-collect))))
  (setq gc-cons-threshold 134217728))
