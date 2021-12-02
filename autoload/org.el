;;; autoload/org.el -*- lexical-binding: t; -*-

;;;###autoload
(defun ale-org-font-setup ()
  (interactive)
  (variable-pitch-mode)
  (org-indent-mode)
  (visual-line-mode)
  ;; Centering text
  (when (fboundp 'visual-fill-column-mode)
    (setq visual-fill-column-width 120)
    (setq visual-fill-column-center-text t)
    (visual-fill-column-mode 1))
  ;; Setup font
  (let ((variable-pitch `(:font ,(ale-font-chooser ale-variable-fonts)))
        (default `(:font ,(ale-font-chooser ale-default-fonts))))
    (custom-theme-set-faces
     'user
     `(org-level-4 ((t (,@variable-pitch :height 1.2))))
     `(org-level-3 ((t (,@variable-pitch :height 1.3))))
     `(org-level-2 ((t (,@variable-pitch :height 1.4))))
     `(org-level-1 ((t (,@variable-pitch :height 1.5))))
     `(org-block ((t (,@default))))
     ;; ensure that anything that should be fixed-pitch in Org files appears that way
     '(org-code ((t (:inherit (shadow fixed-pitch)))))
     '(org-formula ((t (:inherit (shadow fixed-pitch)))))
     '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
     '(org-link ((t (:foreground "royal blue" :underline t))))
     '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     '(org-property-value ((t (:inherit fixed-pitch))) t)
     '(org-checkbox ((t (:inherit fixed-pitch))) t)
     '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     '(org-table ((t (:inherit (shadow fixed-pitch)))))
     '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
     '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))))

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
