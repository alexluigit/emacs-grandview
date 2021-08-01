(push (concat user-emacs-directory "lisp") load-path)

(defvar ale/init-dot-repo (file-truename user-emacs-directory)
  "Get dotfiles repo path before changing `user-emacs-directory'.")
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))

(require 'ale-gc)
(require 'ale-package)

(defun ale/init-build ()
  "doc"
  (require 'ale-files)
  (let* ((init-org (concat ale/init-dot-repo "ale.org"))
         (init-el (concat user-emacs-directory "ale.el"))
         (init-md5 (concat user-emacs-directory "init.md5"))
         (old-md5 (when (file-exists-p init-md5)
                    (ale/files-read init-md5)))
         (new-md5 (secure-hash 'md5 (ale/files-read init-org))))
    (unless (string= old-md5 new-md5)
      (with-temp-buffer
        (erase-buffer)
        (insert new-md5)
        (write-region (point-min) (point-max) init-md5))
      (require 'ob-tangle)
      (org-babel-tangle-file init-org init-el))))

(add-hook 'kill-emacs-hook #'ale/init-build)

(let ((init (concat user-emacs-directory "ale.el")))
  (when (file-exists-p init) (load-file init)))
