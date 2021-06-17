(defvar bootstrap-version)
(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 'full)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(push (concat ace/init-dot-repo "lisp") load-path)

;; Load configurations.
(defun ace/init-load-config ()
  "Load main Emacs configurations, either '.el' or '.org' file."
  (let ((init-el (concat user-emacs-directory "ace.el"))
        (init-org (concat ace/init-dot-repo "ace.org")))
    (if (file-exists-p init-el)
        (load-file init-el)
      (require 'org)
      (org-babel-tangle-file init-org init-el)
      (load-file init-el))))

(ace/init-load-config)

(defun ace/init-build-config ()
  "Automatically tangle main init org file at saving when current buffer name matches.
Add this to `after-save-hook' in `org-mode-hook'."
  (let ((init-el (concat user-emacs-directory "ace.el"))
        (init-org (concat ace/init-dot-repo "ace.org")))
    (org-babel-tangle-file init-org init-el)
    (byte-compile-file init-el)))

(defun ace/init-edit-config ()
  "A util function for finding init file quickly."
  (interactive)
  (find-file (concat ace/init-dot-repo "ace.org")))

(add-hook 'kill-emacs-hook #'ace/init-build-config)
