;;; autoload/org.el -*- lexical-binding: t; -*-

;;;###autoload
(defun ale-org-font-setup ()
  "Setup variable-pitch fonts in org-mode."
  (interactive)
  (variable-pitch-mode)
  (let ((variable-pitch `(:font ,(ale-font-chooser ale-variable-fonts)))
        (default `(:font ,(ale-font-chooser ale-default-fonts))))
    (custom-theme-set-faces
     'user
     `(org-level-1 ((t (,@variable-pitch :height 1.5))))
     `(org-level-2 ((t (,@variable-pitch :height 1.4))))
     `(org-level-3 ((t (,@variable-pitch :height 1.3))))
     `(org-level-4 ((t (,@variable-pitch :height 1.2))))
     `(org-block ((t (,@default :inherit (default shadow)
                                :height 1.0
                                :background ,(face-attribute 'org-block-begin-line :background)
                                :extend t))))
     `(org-block-begin-line ((t (:foreground "#606060" :extend t))))
     '(org-tag ((t (:inherit (shadow) :weight bold :height 0.8)))))))

;;;###autoload
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

(provide 'ale-org)
