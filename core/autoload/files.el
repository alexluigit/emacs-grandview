;;; core/autoload/files.el --- -*- lexical-binding: t -*-

(require 'mailcap)
(eval-when-compile (require 'cl-lib))

(defcustom ale-files-additional-mime '((".ape" . "audio/ape") (".rmvb" . "video/rm") (".f4v" . "video/f4v"))
  "doc")

(defvar ale-files-dot-repo nil
  "doc")

(defcustom ale-files-dir-alist
  '(((title . "  Photos")       (path . "~/Pictures/"))
    ((title . "  Videos")       (path . "~/Video/"))
    ((title . "  Downloads")    (path . "~/Downloads/")))
  "doc")

(defcustom ale-files-cmd-alist
  '(("video/" ("floatwin" "-c" "mpv:emacs-mpv" "mpv" "--x11-name=emacs-mpv" "%f"))
    (("rm" "rmvb") ("floatwin" "-c" "mpv:emacs-mpv" "mpv" "--x11-name=emacs-mpv" "%f")))
  "doc"
  :group 'files :type '(alist :value-type ((choice list string) list)))

;;;###autoload
(defalias 'ale-files-find-dir 'find-file)

;;;###autoload
(with-eval-after-load 'danger
  (defalias 'ale-files-find-dir 'danger-find-file))

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

(defun ale-files-find-file-external (entry &optional cmd args)
  "Open file using external shell command."
  (let ((process-connection-type nil)
        (entry (shell-quote-argument (expand-file-name entry))))
    (unless (executable-find cmd)
      (user-error (format "Install `%s' to preview %s" cmd entry)))
    (setq args (cl-substitute entry "%f" args :test 'string=))
    (let ((default-directory "~"))
      (apply #'start-process "" nil "nohup" (append (list cmd) args)))))

(defun ale-files-find-file-advisor (fn file &rest args)
  "Advisor of `find-file' that opens some types of file externally."
  (if-let ((match (ale-files-match-mime file)))
      (let ((cmd (car match))
            (args (cdr match)))
        (add-to-list 'recentf-list file)
        (ale-files-find-file-external file cmd args))
    (apply fn file args)))

(advice-add #'find-file :around #'ale-files-find-file-advisor)
(advice-add #'find-file-other-window :around #'ale-files-find-file-advisor)

;;;###autoload
(defun ale-files-in-user-dirs ()
  "Open files in directories defined in `ale-files-dir-alist'."
  (interactive)
  (let* ((cands-raw (mapcar (lambda (i) (cdr (assq 'title i))) ale-files-dir-alist))
         (get-item (lambda (s field) (cl-dolist (i ale-files-dir-alist)
                                 (when (string= s (cdr (assq 'title i)))
                                   (cl-return (cdr (assq field i)))))))
         (annotation (lambda (s) (marginalia--documentation (funcall get-item s 'path))))
         (cands (ale-minibuffer-append-metadata annotation cands-raw))
         (title (completing-read "Open: " cands nil t))
         (path (funcall get-item title 'path)))
    (ale-minibuffer--files-in-directory path (concat title ": "))))

;;;###autoload
(defun ale-files-dotfiles ()
  "Open files in dotfiles repo."
  (interactive)
  (unless ale-files-dot-repo
    (user-error "`ale-files-dot-repo' is undefined"))
  (ale-minibuffer--files-in-directory ale-files-dot-repo " Dotfiles: "))

;;;###autoload
(defun ale-files-edit-emacs-config ()
  "Editing emacs init file."
  (interactive)
  (find-file (concat ale-init-directory "ale.org")))

;;;###autoload
(defun ale-files-browse-all-directories ()
  "Browse all directories using `fd' command."
  (interactive)
  (let* ((command "fd -H -td -0 . /")
         (output (shell-command-to-string command))
         (files-raw (split-string output "\0" t))
         (files (ale-minibuffer-append-metadata 'file files-raw))
         (file (completing-read "Goto: " files)))
    (ale-files-find-dir file)))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun ale-files-update ()
  "Update current buffer."
  (interactive)
  (if (derived-mode-p 'helpful-mode)
      (helpful-update)
    (revert-buffer)))

;;;###autoload
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
