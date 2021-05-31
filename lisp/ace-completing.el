;;; -*- lexical-binding: t -*-

(defcustom ace/user-dir-alist '(
  ((title . "  System Files") (path . "/home/alex/Code/alex.files/"))
  ((title . "  Coding")   (path . "/media/HDD/Dev/"))
  ((title . "  Books")        (path . "/media/HDD/Book/"))
  ((title . "  Notes")        (path . "/home/alex/Documents/notes/"))
  ((title . "  Photos")       (path . "/home/alex/Pictures/"))
  ((title . "  Videos")       (path . "/home/alex/Video/"))
  ((title . "  Downloads")    (path . "/home/alex/Downloads/")))
  "doc")

;;;###autoload
(defun ace/completing-append-metadata (meta completions)
  "doc"
  (let ((entry (if (functionp meta)
                   `(metadata (annotation-function . ,meta))
                 `(metadata (category . ,meta)))))
    (lambda (string pred action)
      (if (eq action 'metadata)
          entry
        (complete-with-action action completions string pred)))))

(defun ace/completing-word-ispell ()
  "Completings the symbol at point based on entries in the
dictionary"
  (interactive)
  (when-let* ((word (thing-at-point 'symbol t))
              (boundaries (bounds-of-thing-at-point 'symbol))
              (start (car boundaries))
              (end (cdr boundaries))
              (words (ispell-lookup-words word))
              (selection (completing-read "Words: " words)))
    (delete-region start end) (insert selection)))

(defun ace/completing-read (prompt collection &rest comp-read-args)
  "Calls `completing-read' but returns the value from COLLECTION.

Simple wrapper around the `completing-read' function that assumes
the collection is either an alist, or a hash-table, and returns
the _value_ of the choice, not the selected choice."
  (cl-flet ((assoc-list-p (obj) (and (listp obj) (consp (car obj)))))
    (let* ((choice (apply #'completing-read prompt collection comp-read-args))
           (results (cond
                     ((hash-table-p collection) (gethash choice collection))
                     ((assoc-list-p collection) (alist-get choice collection nil nil 'equal))
                     (t                         choice))))
      (if (listp results) (car results) results))))

(defun ace/completing-murl (&optional no-hist)
  "Select video or stream to play in mpv."
  (interactive "P")
  (let* ((list-file "/home/alex/.cache/murl/main_list.json")
         (playlist (append (json-read-file list-file) nil)))
    (unless no-hist
      (when-let* ((clip (condition-case nil (current-kill 0 t) (error "")))
                  (remove-syntax (set-text-properties 0 (length clip) nil clip))
                  (is-url (string-prefix-p "http" clip))
                  (json (shell-command-to-string (concat "murl json " clip)))
                  (valid (string-prefix-p "{" json))
                  (obj (json-read-from-string json)))
        (cl-pushnew obj playlist :test 'equal)
        (f-write-text (json-encode (vconcat playlist)) 'utf-8 list-file)))
    (let* ((cands-raw (mapcar (lambda (i) (cdr (assq 'title i))) playlist))
           (get-url (lambda (s) (cl-dolist (i playlist)
                                   (when (string= s (cdr (assq 'title i)))
                                     (cl-return (cdr (assq 'url i)))))))
           (annotation (lambda (s) (marginalia--documentation (funcall get-url s))))
           (cands (ace/completing-append-metadata annotation cands-raw))
           (title (completing-read "murls: " cands)))
      (call-process "murl" nil 0 nil (funcall get-url title) "true"))))

(defun ace/completing-file-video-p (file)
  "Determine if FILE is a video file."
  (with-temp-buffer
    (let ((exit (call-process "file" nil t nil "-bi" file))
          (ext (file-name-extension file))
          (extra-exts '("rmvb" "rm" "ts")))
      (when (not (zerop exit))
        (signal 'file-mime-type-error (list "Command failed" (buffer-string))))
      (let ((str (buffer-string)))
        (and (string-match "charset=binary" str)
             (or (string-match "video/" str) (member ext extra-exts)))))))

(defun ace/completing-file-in-user-dirs ()
  "Select video or stream to play in mpv."
  (interactive)
  (let* ((cands-raw (mapcar (lambda (i) (cdr (assq 'title i))) ace/user-dir-alist))
         (get-item (lambda (s field) (cl-dolist (i ace/user-dir-alist)
                                 (when (string= s (cdr (assq 'title i)))
                                   (cl-return (cdr (assq field i)))))))
         (annotation (lambda (s) (marginalia--documentation (funcall get-item s 'path))))
         (cands (ace/completing-append-metadata annotation cands-raw))
         (title (completing-read "Open: " cands))
         (path (funcall get-item title 'path)))
    (ace/completing--files-in-directory path (concat title ": "))))

(defun ace/completing--files-in-directory (dir &optional prompt)
  "Use `fd' to list files in DIR."
  (let* ((default-directory dir)
         (command "fd -H -t f -0")
         (output (shell-command-to-string command))
         (files-raw (split-string output "\0" t))
         (files (ace/completing-append-metadata 'file files-raw))
         (file (completing-read (or prompt "Open file: ") files)))
    (if (ace/completing-file-video-p file)
        (let* ((file (shell-quote-argument (concat dir file)))
               (floatw-flags (list "floatwin" "-c" "mpv:emacs-mpv"))
               (mpv-flags (list "mpv" "--x11-name=emacs-mpv" file))
               (flags (append floatw-flags mpv-flags)))
          (apply #'start-process "" nil "nohup" flags))
      (find-file file))
    (delete-file (concat dir "nohup.out") t)))

(provide 'ace-completing)
