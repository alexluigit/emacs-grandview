;; Typescript
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq lsp-eslint-server-command
  '("node"
  "/home/alex/.local/share/lsp/eslint/server/out/eslintServer.js" "--stdio"))
  (setq typescript-indent-level 2))

;; Vue
(use-package vue-mode
  :mode "\\.vue\\'"
  ;; :hook (vue-mode . lsp-deferred)
  :config
  (add-hook 'vue-mode-hook #'lsp))
