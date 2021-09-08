;;; init.el --- -*- lexical-binding: t -*-

(let ((init-dir (file-name-directory user-init-file)))
  (load (concat init-dir "init/vars.el") nil t)
  (load (concat init-dir "init/package.el") nil t)
  (load (concat init-dir "init/tangle.el") nil t)
  (load (concat init-dir "core/editor.el") nil t)
  (load (concat init-dir "core/completion.el") nil t)
  (load (concat init-dir "core/ui.el") nil t)
  (load (concat init-dir "core/bindings.el") nil t))
