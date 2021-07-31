;;; ale-lsp.el --- -*- lexical-binding: t -*-

(defvar ale/lsp-enable-languages
  '(sh lua haskell ale/vue typescript rust)
  "doc")

(defun ale/lsp--inhibit ()
  "Disable `lsp-deferred' in minibuffer."
  (advice-add 'lsp-deferred :override #'ignore))

(defun ale/lsp--recover ()
  "Recover `lsp-deferred' after quit minibuffer."
  (advice-remove 'lsp-deferred #'ignore))

(define-minor-mode ale/lsp-mode
  "Inhibit lsp in minibuffer."
  :init-value nil
  :global t
  (if ale/lsp-mode
      (progn
      (add-hook 'minibuffer-setup-hook 'ale/lsp--inhibit)
      (add-hook 'minibuffer-exit-hook 'ale/lsp--recover)
      (dolist (lang ale/lsp-enable-languages)
        (add-hook (intern (format "%s-mode-hook" lang)) #'lsp-deferred)))
    (progn
      (remove-hook 'minibuffer-setup-hook 'ale/lsp--inhibit)
      (remove-hook 'minibuffer-exit-hook 'ale/lsp--recover)
      (dolist (lang ale/lsp-enable-languages)
        (remove-hook (intern (format "%s-mode-hook" lang)) #'lsp-deferred)))))

(provide 'ale-lsp)
