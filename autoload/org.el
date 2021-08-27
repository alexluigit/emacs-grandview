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
  (let ((var-font `(:font ,(ale-font-chooser ale-org-fonts))))
    (custom-theme-set-faces
     'user
     `(org-level-4 ((t (,@var-font :height 1.2))))
     `(org-level-3 ((t (,@var-font :height 1.3))))
     `(org-level-2 ((t (,@var-font :height 1.4))))
     `(org-level-1 ((t (,@var-font :height 1.5))))
     ;; ensure that anything that should be fixed-pitch in Org files appears that way
     '(org-block ((t (:inherit fixed-pitch))))
     '(org-code ((t (:inherit (shadow fixed-pitch)))))
     '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
     '(org-link ((t (:foreground "royal blue" :underline t))))
     '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     '(org-property-value ((t (:inherit fixed-pitch))) t)
     '(org-checkbox ((t (:inherit fixed-pitch))) t)
     '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
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

;;;###autoload
(with-eval-after-load 'org
  (define-key org-mode-map [remap org-toggle-comment] 'ale-org-comment-entry-in-region))

(provide 'ale-org)
