;;; init.el --- tangled from grandview.org  -*- lexical-binding: t -*-

;;; Commentary:
;; This file is auto-generated.
;; Do *NOT* edit this file directly.  Edit relevant sections in `grandview.org' instead.
;; Do not delete this file, otherwise you'll have to retangle it manually.

;;; Code:

(defvar grandview--cache
  (expand-file-name "grandview/" user-emacs-directory))

(defvar grandview--def-dir
  (expand-file-name "autoloads/" grandview--cache))

(defun grandview--path (type)
  "Get grandview's init path according to TYPE."
  (pcase type
    ('g (expand-file-name "grandview.org"
                          (file-name-directory user-init-file)))
    ('private (expand-file-name "grandview.private.el"
                                (file-name-directory user-init-file)))
    ('main (expand-file-name "grandview.el" grandview--cache))
    ('main-md5 (expand-file-name "grandview.el.md5" grandview--cache))
    ('def-el (expand-file-name "grandview-loaddefs.el" grandview--cache))
    ('def-md5 (expand-file-name "grandview-loaddefs.md5" grandview--cache))))

(defun grandview--readfile (path)
  "Return the decoded text in PATH as single-byte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (when (file-exists-p path) (insert-file-contents-literally path))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun grandview--put-tangle-path (file-name)
  "Prepare metadata for `grandview-tangle' in FILE-NAME."
  (with-current-buffer (find-file-noselect file-name)
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
    (kill-current-buffer)))

(defun grandview--loaddefs-gen (&optional force)
  "Generate autoload files for Grandview.
Only do it when FORCE or contents in autoload directory changed."
  (let* ((autoload-md5 (grandview--path 'def-md5))
         (old-md5 (grandview--readfile autoload-md5))
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
  "Tangle grandview.org to grandview.el.
The tanglement only happens when the hashed md5 string changed after
last change or FORCE is non nil."
  (let* ((g-org (grandview--path 'g))
         (main-el (grandview--path 'main))
         (md5-file (grandview--path 'main-md5))
         (old-md5 (grandview--readfile md5-file))
         (new-md5 (secure-hash 'md5 (grandview--readfile g-org)))
         find-file-hook kill-buffer-hook write-file-functions
         enable-local-variables)
    (when (or force (not (string= old-md5 new-md5)))
      (require 'ob-tangle)
      (grandview--put-tangle-path g-org)
      (with-temp-buffer
        (erase-buffer)
        (insert new-md5)
        (write-region (point-min) (point-max) md5-file))
      (let (org-confirm-babel-evaluate)
        (org-babel-tangle-file g-org main-el)
        (with-temp-buffer
          (insert-file-contents main-el)
          (goto-char (point-max))
          (insert "\n(provide 'grandview)\n;;; grandview.el ends here")
          (write-region nil nil main-el)))
      (cl-loop for lib in (directory-files-recursively
                           grandview--def-dir "\\.el$")
               for base = (file-name-base lib)
               for end-s =
               (format "\n(provide '%s)\n;;; %s.el ends here" base base)
               do (with-temp-buffer
                    (insert ";;; -*- lexical-binding: t -*-\n\n")
                    (insert-file-contents lib)
                    (goto-char (point-max))
                    (insert end-s)
                    (write-region nil nil lib)))
      (grandview--loaddefs-gen force))))

(let ((raise-err-when-debug-init (not (getenv-internal "DEBUG")))
      file-name-handler-alist)
  (unless (file-exists-p grandview--cache)
    (make-directory grandview--def-dir t)
    (grandview-tangle t)) ; "Initiate spin!" -- Joseph Cooper
  (add-to-list 'load-path grandview--cache)
  (add-to-list 'load-path grandview--def-dir)
  (add-hook 'kill-emacs-hook #'grandview-tangle -90)
  (require 'grandview-macros nil raise-err-when-debug-init)
  (require 'grandview-custom nil raise-err-when-debug-init)
  (when-let* ((private-conf (grandview--path 'private))
              ((file-exists-p private-conf)))
    (load private-conf raise-err-when-debug-init 'silent))
  (require 'grandview-loaddefs nil raise-err-when-debug-init)
  (push '(grandview-org-local-setup) safe-local-eval-forms)
  (require 'grandview nil raise-err-when-debug-init)
  (setq gc-cons-threshold 134217728))
