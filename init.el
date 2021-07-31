(push (concat user-emacs-directory "lisp") load-path)

(defvar ale/init-dot-repo (file-truename user-emacs-directory)
  "Get dotfiles repo path before changing `user-emacs-directory'.")
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))

(require 'ale-gc)
(require 'ale-package)
(autoload 'ale/files-read "ale-files")

(defun ale/init-modified-p ()
  (let* ((init-md5-file (concat user-emacs-directory "init-org.md5"))
         (init-org-file (concat ale/init-dot-repo "ale.org"))
         (old-md5 (when (file-exists-p init-md5-file)
                    (ale/files-read init-md5-file)))
         (new-md5 (secure-hash 'md5 (ale/files-read init-org-file))))
    (with-temp-buffer
      (erase-buffer)
      (insert new-md5)
      (write-region (point-min) (point-max) init-md5-file))
    (not (string= old-md5 new-md5))))

(defun ale/init-load-config ()
  "Tangle (if needed) `ale.org' and load `ale.el'."
  (let ((init-el (concat user-emacs-directory "ale.el"))
        (init-org (concat ale/init-dot-repo "ale.org")))
    (when (or (ale/init-modified-p)
              (not (file-exists-p init-el)))
      (require 'ob-tangle)
      (org-babel-tangle-file init-org init-el))
    (load-file init-el)))

(ale/init-load-config)
