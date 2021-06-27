(require 'ale-minibuffer)
(require 'mailcap)
(eval-when-compile (require 'cl-lib))

(defcustom ale/files-additional-mime '((".ape" . "audio/ape") (".rmvb" . "video/rm") (".f4v" . "video/f4v"))
  "doc")

(defcustom ale/files-dir-alist
  '(((title . "  System Files") (path . "/home/alex/Code/alex.files/"))
    ((title . "  Shows")        (path . "/media/HDD/Share/"))
    ((title . "  Coding")       (path . "/media/HDD/Dev/"))
    ((title . "  Books")        (path . "/media/HDD/Book/"))
    ((title . "  Notes")        (path . "/home/alex/Documents/notes/"))
    ((title . "  Photos")       (path . "/home/alex/Pictures/"))
    ((title . "  Videos")       (path . "/home/alex/Video/"))
    ((title . "  Movies")       (path . "/media/Cloud/共享/Movies/"))
    ((title . "  Downloads")    (path . "/home/alex/Downloads/")))
  "doc")

;; Copied from `f.el'
(defun ale/files--read-bytes (path)
  "Read binary data from PATH.

Return the binary data as unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (setq buffer-file-coding-system 'binary)
    (insert-file-contents-literally path)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ale/files-read (path &optional coding)
  "Read text with PATH, using CODING.

CODING defaults to `utf-8'.
Return the decoded text as multibyte string."
  (decode-coding-string (ale/files--read-bytes path) (or coding 'utf-8)))

(defcustom ale/files-cmd-alist
  '(("video/"           ("floatwin" "-c" "mpv:emacs-mpv" "mpv" "--x11-name=emacs-mpv" "%f"))
    (("ts" "rm" "rmvb") ("floatwin" "-c" "mpv:emacs-mpv" "mpv" "--x11-name=emacs-mpv" "%f")))
  "doc"
  :group 'lf :type '(alist :value-type ((choice list string) list)))

(cl-defun ale/files-match-mime (file)
  "To determine if `FILE' can be matched by `ale/files-cmd-alist'."
  (let ((meta (with-temp-buffer (call-process "file" nil t nil "-bi" file) (buffer-string))))
    (when (or (not (string-match "charset=binary" meta))
              (string-match "inode/x-empty" meta))
      (cl-return-from ale/files-match-mime))
    (pcase-dolist (`(,re-or-exts ,cmd) ale/files-cmd-alist)
      (if (listp re-or-exts)
          (let ((ext (file-name-extension file)))
            (when (member ext re-or-exts)
              (cl-return-from ale/files-match-mime cmd)))
        (when (string-match re-or-exts meta)
          (cl-return-from ale/files-match-mime cmd))))))

(defun ale/files-find-file-external (entry &optional cmd args)
  "Open file using external shell command."
  (let ((process-connection-type nil)
        (entry (shell-quote-argument entry)))
    (unless (executable-find cmd)
      (user-error (format "Install `%s' to preview %s" cmd entry)))
    (setq args (cl-substitute entry "%f" args :test 'string=))
    (let ((default-directory "~"))
      (apply #'start-process "" nil "nohup" (append (list cmd) args)))))

(defun ale/files-find-file-advisor (fn file &rest args)
  "Advisor of `find-file' that opens some types of file externally."
  (if-let ((match (ale/files-match-mime file)))
      (let ((cmd (car match))
            (args (cdr match)))
        (add-to-list 'recentf-list file)
        (ale/files-find-file-external file cmd args))
    (apply fn file args)))

(advice-add #'find-file :around #'ale/files-find-file-advisor)

(defun ale/files-in-user-dirs (&optional skip-menu)
  "Select video or stream to play in mpv."
  (interactive (list (if current-prefix-arg "  System Files" nil)))
  (when skip-menu (setq skip-menu "  System Files"))
  (let* ((cands-raw (mapcar (lambda (i) (cdr (assq 'title i))) ale/files-dir-alist))
         (get-item (lambda (s field) (cl-dolist (i ale/files-dir-alist)
                                 (when (string= s (cdr (assq 'title i)))
                                   (cl-return (cdr (assq field i)))))))
         (annotation (lambda (s) (marginalia--documentation (funcall get-item s 'path))))
         (cands (ale/minibuffer-append-metadata annotation cands-raw))
         (title (or skip-menu (completing-read "Open: " cands)))
         (path (funcall get-item title 'path)))
    (ale/minibuffer--files-in-directory path (concat title ": "))))

(defun ale/files-recent ()
  "Open file from `recentf-list' with completion."
  (interactive)
  (when-let* ((cands-raw (mapcar #'abbreviate-file-name recentf-list))
              (cands (ale/minibuffer-append-metadata 'file cands-raw))
              (fname (completing-read "File name: " cands nil nil)))
    (find-file (expand-file-name fname))))

(defun ale/files-other-window ()
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
                      (windmove-right)
                      (find-file p)
                      (meow--update-cursor))
                    cand)
    (keyboard-escape-quit)))

(mailcap-parse-mimetypes)
(cl-dolist (mm ale/files-additional-mime)
  (add-to-list 'mailcap-mime-extensions mm))

(provide 'ale-files)
