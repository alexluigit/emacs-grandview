;;; ale-murl.el -*- lexical-binding: t; -*-

(require 'json)

;;;###autoload
(defun ale/murl-open (&optional no-hist)
  "Select video or stream to play in mpv."
  (interactive "P")
  (let* ((list-file "/home/alex/.cache/murl/main_list.json")
         (playlist (append (json-read-file list-file) nil))
         (clip (condition-case nil (current-kill 0 t) (error ""))))
    (unless no-hist
      (set-text-properties 0 (length clip) nil clip)
      (when-let* ((is-url (string-prefix-p "http" clip))
                  (json (shell-command-to-string (concat "murl -P 1088 json '" clip "'")))
                  (valid (string-prefix-p "{" json))
                  (obj (json-read-from-string json)))
        (cl-pushnew obj playlist :test 'equal)
        (with-temp-buffer
          (insert (json-encode (vconcat playlist)))
          (json-pretty-print-buffer)
          (write-region (point-min) (point-max) list-file))))
    (let* ((cands-raw (mapcar (lambda (i) (cdr (assq 'title i))) playlist))
           (get-url (lambda (s) (cl-dolist (i playlist)
                                   (when (string= s (cdr (assq 'title i)))
                                     (cl-return (cdr (assq 'url i)))))))
           (annotation (lambda (s) (marginalia--documentation (funcall get-url s))))
           (cands (ale-minibuffer-append-metadata annotation cands-raw))
           (title (completing-read "murls: " cands)))
      (call-process "murl" nil 0 nil "-r" "-P" "1088" "-d" "40%x40%+2300+10" (funcall get-url title)))))

(provide 'ale-murl)
