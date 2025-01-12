;;; init.el --- -*- lexical-binding: t -*-

(defvar grandview--cache
  (expand-file-name "grandview/" user-emacs-directory))

(defvar grandview--def-dir
  (expand-file-name "autoloads/" grandview--cache))

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
    ('main (expand-file-name "grandview.el" grandview--cache))
    ('main-md5 (expand-file-name "grandview.el.md5" grandview--cache))
    ('def-el (expand-file-name "grandview-loaddefs.el" grandview--cache))
    ('def-md5 (expand-file-name "grandview-loaddefs.md5" grandview--cache))))

(defun grandview--gen-tangle-path ()
  "Prepare metadata for `grandview-tangle'."
  (with-current-buffer (find-file-noselect grandview--dot-org)
    (goto-char (point-min))
    (save-excursion
      (widen)
      (cl-labels
          ((tangle-path-template ()
             ":tangle (expand-file-name \"%s\" grandview--def-dir)")
           (put-tangle-path (pkg)
             (let* ((pfx+? (ignore-errors (string= "+" (substring pkg 0 1))))
                    (.el? (ignore-errors
                            (string= ".el" (substring pkg -3))))
                    (f-name (concat (if pfx+? "" "+") pkg (if .el? "" ".el")))
                    (path (format (tangle-path-template) f-name)))
               (org-entry-put nil "header-args:emacs-lisp" path))))
        (org-map-entries
         (lambda ()
           (org-with-point-at (point)
             (when-let* ((heading (org-get-heading))
                         ((string-prefix-p "Extras" heading)))
               (cond
                ((string-prefix-p "Extras ::: " heading)
                 (put-tangle-path
                   (cadr (string-split heading " ::: "))))
		((string= heading "Extras")
                 (let* ((title (save-excursion
                                 (org-up-heading-safe) (org-get-heading)))
                        (memo (ignore-errors
                                (substring
                                 title (1+ (string-match "(\\(.*\\))" title))
                                 (match-end 1)))))
                   (put-tangle-path
                    (or memo (replace-regexp-in-string
                              (regexp-quote " ") "_" title t t))))))))))))
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
                           grandview--def-dir "\\.el$")
               for f-base = (file-name-base lib)
               for f-name = (file-name-nondirectory lib)
               for end-s =
               (format "\n(provide '%s)\n;;; %s ends here" f-base f-name)
               do (with-temp-buffer
                    (insert ";;; -*- lexical-binding: t -*-\n\n")
                    (insert-file-contents lib)
                    (goto-char (point-max))
                    (insert end-s)
                    (write-region nil nil lib))))))

(defun grandview--gen-autoload (&optional force)
  "Generate autoload files for Grandview.
Only do it when FORCE or contents in autoload directory changed."
  (let* ((autoload-md5 (grandview--path 'def-md5))
         (old-md5 (when (file-exists-p autoload-md5)
                    (grandview--readfile autoload-md5)))
         (all-el-files (directory-files-recursively
                        grandview--def-dir "\\.el$"))
         (files-as-str (with-temp-buffer
                         (dolist (file all-el-files)
                           (insert-file-contents file))
                         (buffer-string)))
         (new-md5 (secure-hash 'md5 files-as-str)))
    (when (or force (not (string= old-md5 new-md5)))
      (let ((generate-autoload-file nil) (inhibit-message t))
        (loaddefs-generate grandview--def-dir
                           (grandview--path 'def-el)))
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
  (unless (file-exists-p grandview--cache)
    (make-directory grandview--def-dir t)
    (grandview-tangle t)) ; "Initiate spin!" -- Joseph Cooper
  (add-to-list 'load-path grandview--cache)
  (add-to-list 'load-path grandview--def-dir)
  (add-hook 'kill-emacs-hook #'grandview-tangle -90)
  (require 'grandview-macros nil (not debug))
  (require 'grandview-custom nil (not debug))
  (require 'grandview-loaddefs nil (not debug))
  (require 'grandview nil (not debug))
  (setq gc-cons-threshold 134217728))
