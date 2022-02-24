;;; init/files.el --- -*- lexical-binding: t -*-

(require 'mailcap)
(eval-when-compile (require 'cl-lib))

(defadvice! +find-library-ad (fn &rest args)
  "Always follow symlink when using `find-library'.

Package managers like `straight.el' use symlink to manage
package/libraries. This advice will enable user always find
libraries's truename."
  :around #'find-library
  (let ((vc-follow-symlinks t)) (apply fn args)))

(defadvice! +files-find-file-ad (fn file &rest args)
  "Advisor of `find-file' that opens some types of file externally."
  :around #'find-file
  :around #'find-file-other-window
  (if-let ((match (ale-files-match-mime file)))
      (let ((cmd (car match))
            (args (cdr match)))
        (add-to-list 'recentf-list file)
        (ale-files-find-file-external file cmd args))
    (apply fn file args)))

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
         (files (completion-append-metadata! 'file files-raw))
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
         (cands (completion-append-metadata! annotation cands-raw))
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
  (find-file ale-full-config-org)
  (setq-local completion-at-point-functions '(elisp-completion-at-point t)))

(defun ale-files-other-window ()
  "Doc."
  (interactive)
  (let* ((meta (completion-metadata
                (buffer-substring-no-properties (field-beginning) (point))
                minibuffer-completion-table
                minibuffer-completion-predicate))
         (category (completion-metadata-get meta 'category))
         (category-file (memq category '(file project-file)))
         (cand (vertico--candidate)))
    (unless category-file (user-error "Candidate is not a file"))
    (if (eq category 'project-file)
        (setq cand (expand-file-name cand (or (cdr-safe (project-current))
                                              (car (minibuffer-history-value)))))
      (setq cand (expand-file-name cand)))
    (run-with-timer 0.1 nil
                    (lambda (p)
                      (split-window-right)
                      (other-window 1)
                      (find-file p)
                      (meow--update-cursor))
                    cand)
    (keyboard-escape-quit)))

(defun ale-files-revert-buffer-no-ask ()
  "Revert buffer, no ask for confirmation."
  (interactive)
  (revert-buffer nil t))

(defun ale-files-rename-file-and-buffer (name)
  "Apply NAME to current file and rename its buffer.
Do not try to make a new directory or anything fancy."
  (interactive
   (list (read-string "Rename current file: " (buffer-file-name))))
  (let* ((file (buffer-file-name)))
    (if (vc-registered file)
        (vc-rename-file file name)
      (rename-file file name))
    (set-visited-file-name name t t)))

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
