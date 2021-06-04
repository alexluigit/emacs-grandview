(require 'mailcap)
(eval-when-compile (require 'cl-lib))

(defcustom ace/files-dir-alist
  '(((title . "  System Files") (path . "/home/alex/Code/alex.files/"))
    ((title . "  Coding")       (path . "/media/HDD/Dev/"))
    ((title . "  Books")        (path . "/media/HDD/Book/"))
    ((title . "  Notes")        (path . "/home/alex/Documents/notes/"))
    ((title . "  Photos")       (path . "/home/alex/Pictures/"))
    ((title . "  Videos")       (path . "/home/alex/Video/"))
    ((title . "  Movies")       (path . "/media/Cloud/共享/Movies/"))
    ((title . "  Downloads")    (path . "/home/alex/Downloads/")))
  "doc")

(defcustom ace/files-cmd-alist
  '(("video/"            ("floatwin" "-c" "mpv:emacs-mpv" "mpv" "--x11-name=emacs-mpv" "%f"))
    (("ts" "rm" "rmvb")  ("floatwin" "-c" "mpv:emacs-mpv" "mpv" "--x11-name=emacs-mpv" "%f")))
  "doc"
  :group 'lf :type '(alist :value-type ((choice list string) list)))

(cl-defun ace/files-match-mime (file)
  "To determine if `FILE' can be matched by `ace/files-cmd-alist'."
  (let ((meta (with-temp-buffer (call-process "file" nil t nil "-bi" file) (buffer-string))))
    (when (or (not (string-match "charset=binary" meta))
              (string-match "inode/x-empty" meta))
      (cl-return-from ace/files-match-mime)))
  (pcase-dolist (`(,re-or-exts ,cmd) ace/files-cmd-alist)
    (if (listp re-or-exts)
        (let ((ext (file-name-extension file)))
          (when (member ext re-or-exts)
            (cl-return-from ace/files-match-mime cmd)))
      (when (string-match re-or-exts (or (mailcap-file-name-to-mime-type file) ""))
        (cl-return-from ace/files-match-mime cmd)))))

(defun ace/files-find-file-external (entry &optional cmd args)
  "Open file using external shell command."
  (let ((process-connection-type nil)
        (entry (shell-quote-argument entry)))
    (unless (executable-find cmd)
      (user-error (format "Install `%s' to preview %s" cmd entry)))
    (setq args (cl-substitute entry "%f" args :test 'string=))
    (let ((default-directory "~"))
      (apply #'start-process "" nil "nohup" (append (list cmd) args)))))

(defun ace/files-find-file-advisor (fn file &rest args)
  "Advisor of `find-file' that opens some types of file externally."
  (if-let ((match (ace/files-match-mime file)))
      (let ((cmd (car match))
            (args (cdr match)))
        (add-to-list 'recentf-list file)
        (ace/files-find-file-external file cmd args))
    (apply fn file args)))

(advice-add #'find-file :around #'ace/files-find-file-advisor)

;;;###autoload
(mailcap-parse-mimetypes)
(provide 'ace-files)
