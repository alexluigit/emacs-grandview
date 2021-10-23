;;; init.el --- -*- lexical-binding: t -*-

(let* ((init-dir (file-name-directory user-init-file))
       (user-conf (concat init-dir "user.el"))
       (file-name-handler-alist nil))
  (load (concat init-dir "init/options.el") nil t)
  (load (concat init-dir "init/vars.el") nil t)
  (load (concat init-dir "init/package.el") nil t)
  (load (concat init-dir "init/tangle.el") nil t)
  (load ale-autoload-file nil t)
  (when (file-exists-p user-conf) (load user-conf nil t))
  (load (ale-minimal-config) nil t)
  (when IS-GUI (load ale-full-config nil t))
  (load (concat init-dir "init/gc.el") nil t))
