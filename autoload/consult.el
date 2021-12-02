;;; autoload/consult.el --- -*- lexical-binding: t -*-

(require 'consult)
(autoload 'consult-org "consult-org")

(defun ale-consult-ripgrep-current-file ()
  "Call `consult-ripgrep' for the current buffer (a single file)."
  (interactive)
  (let ((consult-project-root-function #'ignore)
        (consult-ripgrep-args
         (concat "rg "
                 "--null "
                 "--line-buffered "
                 "--color=never "
                 "--line-number "
                 "--smart-case "
                 "--no-heading "
                 "--max-columns=1000 "
                 "--max-columns-preview "
                 "--search-zip "
                 "--with-filename "
                 (shell-quote-argument buffer-file-name))))
    (consult-ripgrep)))

;;;###autoload
(defun ale-consult-line-advisor (fn &rest args)
  "An advice for `consult-line'.

When in a very large file (total lines > 100000), call
`consult-ripgrep' on current file, otherwise execute it
directly."
  (interactive)
  (let ((total-lines (count-lines (point-min) (point-max))))
    (if (> total-lines 100000)
        (ale-consult-ripgrep-current-file)
      (apply fn args))))

;;;###autoload
(defun ale-consult-outline-advisor (fn &rest args)
  "An advice for `consult-outline'.

When in `org-mode', call `consult-org-heading', otherwise call
`consult-oulline'."
  (if (derived-mode-p 'org-mode)
      (consult-org-heading)
    (apply fn args)))

;;;###autoload
(defun ale-consult-project-root ()
  (when-let (project (project-current))
    (car (project-roots project))))
