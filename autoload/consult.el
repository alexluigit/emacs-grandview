;;; autoload/consult.el --- -*- lexical-binding: t -*-

;;;###autoload
(defun ale-consult-line-advisor (fn &rest args)
  "A `consult-line' wrapper.

When in a very large file (total lines > 100000), use a poor man's
version of `consult-line', otherwise execute it directly."
  (interactive)
  (let ((total-lines (bound-and-true-p ale-modeline-total-lines)))
    (unless total-lines
      (setq total-lines (int-to-string (count-lines (point-min) (point-max)))))
    (when (stringp total-lines) (setq total-lines (string-to-number total-lines)))
    (if (> total-lines 100000)
        (let* ((buffer-content (buffer-substring-no-properties (point-min) (point-max)))
               (raw-strs (split-string buffer-content "\n" nil))
               (cand (completing-read "Go: " raw-strs)))
          (beginning-of-buffer)
          (search-forward cand))
      (apply fn args))))

;;;###autoload
(defun ale-consult-switch-buffer-advisor (fn &rest args)
  (interactive)
  (if (car args) (apply fn args) (consult-buffer)))

;;;###autoload
(defun ale-consult-project-root ()
  (when-let (project (project-current))
    (car (project-roots project))))
