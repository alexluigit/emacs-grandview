;;; init.el --- -*- lexical-binding: t -*-

(let ((init-dir (file-name-directory user-init-file))
      (file-name-handler-alist nil))
  (load (concat init-dir "init/options.el") nil t)
  (load (concat init-dir "init/vars.el") nil t)
  (load (concat init-dir "init/package.el") nil t)
  (load (concat init-dir "init/tangle.el") nil t)
  (load (concat init-dir "init/gc.el") nil t))
