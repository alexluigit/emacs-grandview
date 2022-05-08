;;; user.el --- Grandview private configuration -*- lexical-binding: t -*-

;;;; Grandview specific options
;; (setq grandview-cache-dir (locate-user-emacs-file "grandview/"))
;; (setq grandview-org-file (concat (file-name-directory user-init-file) "grandview.org"))
;; (setq grandview-gc-cons-threshold 134217728)

;;;; Other arbitrary codes to load before loading the main config
;; (defadvice! org-toggle-comment-ad (&rest args)
;;   "Drop-in replacement for `org-toggle-comment'.
;; This allows `org-toggle-comment' to toggle comment for all the
;; entries with the same level in the active region while behaves
;; the same when the region is inactive.  This is useful for
;; debugging code blocks in a org config file."
;;   :around #'org-toggle-comment
;;   (if (region-active-p)
;;       (progn
;;         (exchange-point-and-mark)
;;         (let ((end (region-end)) last-point)
;;           (while (< (point) end)
;;             (setq last-point (point))
;;             (apply args)
;;             (org-forward-heading-same-level 1)
;;             (when (eq last-point (point))
;;               (org-forward-element)))))
;;     (apply args)))

;;;; Packages need more testing to be included in the main config
;; (use-package popper
;;   :bind (("C-`"   . popper-toggle-latest)
;;          ("M-`"   . popper-cycle)
;;          ("C-M-`" . popper-toggle-type))
;;   :init
;;   (appendq! popper-reference-buffers
;;             '("Output\\*$" "\\*Async Shell Command\\*" compilation-mode))
;;   (popper-mode)
;;   (popper-echo-mode))
