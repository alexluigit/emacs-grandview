;; Org-bullets
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

;; Center Org Buffers
(defun alex/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))
(use-package visual-fill-column
  :hook (org-mode . alex/org-mode-visual-fill))

;; Org-babel
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))
(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; Org-tempo
(require 'org-tempo) ; this is needed as of Org 9.2
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
