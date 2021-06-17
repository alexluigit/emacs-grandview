;;; -*- lexical-binding: t -*-
(require 'json)
(require 'ace-files)

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

(defun ace/completing-file-in-user-dirs (&optional skip-menu)
  "Select video or stream to play in mpv."
  (interactive (list (if current-prefix-arg "  System Files" nil)))
  (when skip-menu (setq skip-menu "  System Files"))
  (let* ((cands-raw (mapcar (lambda (i) (cdr (assq 'title i))) ace/files-dir-alist))
         (get-item (lambda (s field) (cl-dolist (i ace/files-dir-alist)
                                 (when (string= s (cdr (assq 'title i)))
                                   (cl-return (cdr (assq field i)))))))
         (annotation (lambda (s) (marginalia--documentation (funcall get-item s 'path))))
         (cands (ace/completing-append-metadata annotation cands-raw))
         (title (or skip-menu (completing-read "Open: " cands)))
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
    (find-file (concat dir file))))

(defun ace/completing-recentf ()
  "Open file from `recentf-list' with completion."
  (interactive)
  (when-let* ((cands-raw (mapcar #'abbreviate-file-name recentf-list))
              (cands (ace/completing-append-metadata 'file cands-raw))
              (fname (completing-read "File name: " cands nil nil)))
    (find-file (expand-file-name fname))))

;;;###autoload
(defun ace/completing-buffers-major-mode (&optional arg)
  "Select buffers to switch to. Same as `switch-to-buffer'.
With optional prefix ARG (\\[universal-argument]) select buffers
that match the current buffer's major mode."
  (interactive "P")
  (let* ((major major-mode)
         (prompt "Buffers for"))
    (if arg
        (switch-to-buffer
              (read-buffer
                (format "%s %s:" prompt major) nil t
                (lambda (pair) ; pair is (name-string . buffer-object)
                  (with-current-buffer (cdr pair) (derived-mode-p major)))))
      (switch-to-buffer (read-buffer "Switch to buffer: ")))))

(provide 'ace-completing)
