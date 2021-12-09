;;; init/org.el --- -*- lexical-binding: t -*-

(defvar org-mode-map)

(with-eval-after-load 'org
  (defun ale-org-comment-entry-in-region ()
    "Enhanced drop-in replacement for `org-toggle-comment'."
    (interactive)
    (if (region-active-p)
        (progn
          (exchange-point-and-mark)
          (let ((end (region-end)) last-point)
            (while (< (point) end)
              (setq last-point (point))
              (org-toggle-comment)
              (org-forward-heading-same-level 1)
              (when (eq last-point (point))
                (org-forward-element)))))
      (org-toggle-comment)))
  
  (define-key org-mode-map [remap org-toggle-comment] 'ale-org-comment-entry-in-region)
  (add-hook 'org-tab-first-hook 'org-end-of-line))
