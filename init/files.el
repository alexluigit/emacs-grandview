;;; init/files.el --- -*- lexical-binding: t -*-

(require 'mailcap)
(require 'files)
(eval-when-compile (require 'cl-lib))

(defun ale-files-read-file (path &optional coding)
  "Read text with PATH, using CODING.
CODING defaults to `utf-8'.
Return the decoded text as multibyte string."
  (let ((str (with-temp-buffer
               (set-buffer-multibyte nil)
               (setq buffer-file-coding-system 'binary)
               (insert-file-contents-literally path)
               (buffer-substring-no-properties (point-min) (point-max)))))
  (decode-coding-string str (or coding 'utf-8))))

(defun ale-files-append-meta (metadata candidates)
  "Append METADATA for CANDIDATES."
  (let ((entry (if (functionp metadata)
                   `(metadata (annotation-function . ,metadata))
                 `(metadata (category . ,metadata)))))
    (lambda (string pred action)
      (if (eq action 'metadata)
          entry
        (complete-with-action action candidates string pred)))))

(cl-defun ale-files-match-mime (file)
  "To determine if `FILE' can be matched by `ale-files-cmd-alist'."
  (setq file (expand-file-name file))
  (let ((meta (with-temp-buffer
                (call-process "file" nil t nil "-bi" file) (buffer-string))))
    (when (or (not (string-match "charset=binary" meta))
              (string-match "inode/x-empty" meta))
      (cl-return-from ale-files-match-mime))
    (pcase-dolist (`(,re-or-exts ,cmd) ale-files-cmd-alist)
      (if (listp re-or-exts)
          (let ((ext (file-name-extension file)))
            (when (member ext re-or-exts)
              (cl-return-from ale-files-match-mime cmd)))
        (when (string-match re-or-exts meta)
          (cl-return-from ale-files-match-mime cmd))))))

(defun ale-files-get-all-elisp (dirs)
  (require 'cl-lib)
  (cl-loop for dir in dirs
           append (directory-files-recursively dir "\\.el$") into files
           append files))

(defun ale-files-find-file-external (entry &optional cmd args)
  "Open file using external shell command."
  (let ((process-connection-type nil)
        (entry (shell-quote-argument (expand-file-name entry))))
    (unless (executable-find cmd)
      (user-error (format "Install `%s' to preview %s" cmd entry)))
    (setq args (cl-substitute entry "%f" args :test 'string=))
    (let ((default-directory "~"))
      (apply #'start-process "" nil "nohup" (append (list cmd) args)))))

(defun ale-files--in-directory (dir &optional prompt)
  "Use `fd' to list files in DIR."
  (let* ((default-directory dir)
         (command "fd -H -t f -0")
         (output (shell-command-to-string command))
         (files-raw (split-string output "\0" t))
         (files (ale-files-append-meta 'file files-raw))
         (file (completing-read (or prompt "Open file: ") files)))
    (find-file (concat dir "/" file))))

(defun ale-files-in-user-dirs ()
  "Open files in directories defined in `ale-files-dir-alist'."
  (interactive)
  (let* ((cands-raw (mapcar (lambda (i) (cdr (assq 'title i))) ale-files-dir-alist))
         (get-item (lambda (s field) (cl-dolist (i ale-files-dir-alist)
                                 (when (string= s (cdr (assq 'title i)))
                                   (cl-return (cdr (assq field i)))))))
         (annotation (lambda (s) (marginalia--documentation (funcall get-item s 'path))))
         (cands (ale-files-append-meta annotation cands-raw))
         (title (completing-read "Open: " cands nil t))
         (path (funcall get-item title 'path)))
    (ale-files--in-directory path (concat title ": "))))

(defun ale-files-dotfiles ()
  "Open files in dotfiles repo."
  (interactive)
  (unless ale-files-dot-repo
    (user-error "`ale-files-dot-repo' is undefined"))
  (ale-files--in-directory ale-files-dot-repo " Dotfiles: "))

(defun ale-files-edit-emacs-config ()
  "Editing emacs init file."
  (interactive)
  (find-file ale-full-config-org))

(defun ale-files-revert-buffer-no-ask ()
  "Revert buffer, no ask for confirmation."
  (interactive)
  (revert-buffer nil t))

(defun ale-files-sudo-find ()
  "Reopen current file as root."
  (interactive)
  (let ((file (buffer-file-name)))
  (find-file (if (file-writable-p file)
                 file
               (concat "/sudo::" file)))))

(mailcap-parse-mimetypes)
(cl-dolist (mm ale-files-additional-mime)
  (add-to-list 'mailcap-mime-extensions mm))
