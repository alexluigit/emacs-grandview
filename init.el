;;; init.el --- -*- lexical-binding: t -*-

(let ((init-dir (file-name-directory user-init-file)))
  (load (concat init-dir "init/vars.el") nil t)
  (load (concat init-dir "init/package.el") nil t)
  (load (concat init-dir "init/tangle.el") nil t))
