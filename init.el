(push (concat user-emacs-directory "lisp") load-path)

(defvar ale/init-dot-repo (file-truename user-emacs-directory)
  "Get dotfiles repo path before changing `user-emacs-directory'.")
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))

(require 'ale-gc)
(require 'ale-package)

(defun ale/init-load-config ()
  "Tangle (if needed) `ale.org' and load `ale.el'"
  (let ((init-el (concat user-emacs-directory "ale.el"))
        (init-org (concat ale/init-dot-repo "ale.org")))
    (unless (file-exists-p init-el)
      (require 'ob-tangle)
      (org-babel-tangle-file init-org init-el))
    (load-file init-el)))

(ale/init-load-config)
