;;; autoload/dired.el --- -*- lexical-binding: t -*-

(require 'transient)
(require 'dired)

;;;###autoload
(defun ale-dired-find-file (&rest args)
  (interactive)
  (apply 'find-alternate-file args))

;;;###autoload
(defun ale-dired-file-true-path ()
  (interactive)
  (ale-dired-find-file (file-truename (dired-get-filename))))

;;;###autoload
(defun ale-dired-yank-file-name ()
  "Copy filename under the cursor."
  (interactive)
  (message "Copied: %s" (dired-copy-filename-as-kill)))

;;;###autoload
(defun ale-dired-yank-file-path ()
  "Copy filename under the cursor."
  (interactive)
  (message "Copied: %s" (kill-new (dired-get-filename nil t))))

;;;###autoload
(defun ale-dired-yank-file-dir ()
  "Copy the current directory's (`default-directory''s) absolute path."
  (interactive)
  (message "Copied: %s" (kill-new (expand-file-name default-directory))))

;;;###autoload
(defun ale-dired-browse-all-directories ()
  "Browse all directories using `fd' command."
  (interactive)
  (let* ((command "fd -H -td -0 . /")
         (output (shell-command-to-string command))
         (files-raw (split-string output "\0" t))
         (files (ale-minibuffer-append-metadata 'file files-raw))
         (file (completing-read "Goto: " files)))
    (ale-dired-find-file file)))

;;;###autoload
(defun ale-dired-jump () nil)

;;;###autoload
(defun ale-dired-toolbox () nil)

(defcustom ale-dired-tools '()
  "doc"
  :group 'dired :type 'list
  :set
  (lambda (k v)
    `(setq ,k v)
    (eval `(transient-define-prefix ale-dired-toolbox ()
             ["Select operation for entry: "
              ,@(cl-loop for (key desc fun) in v
                         collect (list key desc fun))]))))

(defcustom ale-dired-routes '()
  "doc"
  :group 'dired :type 'list
  :set
  (lambda (k v)
    `(setq ,k v)
    (eval `(transient-define-prefix ale-dired-jump ()
             ["Go to Directory: "
              ,@(cl-loop for (key desc path) in v
                         collect (list key desc `(lambda () (interactive) (ale-dired-find-file ,path))))]))))

;;;###autoload
(defun ale-dired-rename-space-to-underscore ()
  "Rename current or marked files by replacing space to underscore."
  (interactive)
  (require 'dired-aux)
  (if (derived-mode-p 'dired-mode)
      (let ((markedFiles (dired-get-marked-files )))
        (mapc (lambda (x)
                (when (string-match " " x )
                  (dired-rename-file x (replace-regexp-in-string " " "_" x) nil)))
              markedFiles)
        (when (eq major-mode 'dired-mode) (revert-buffer)))
    (user-error "Not in a dired buffer.")))

;;;###autoload
(defun ale-dired-file-rename-eol ()
  "Rename current file from end of line."
  (interactive)
  (end-of-line)
  (wdired-change-to-wdired-mode)
  (when (featurep 'meow) (meow-append)))

(provide 'ale-dired)
