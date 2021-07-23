(require 'ale-gc)

(defvar bootstrap-version)
(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (let ((url-proxy-services '(("https" . "127.0.0.1:1088"))))
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp))))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(defun ale/init-load-config ()
  "Load main Emacs configurations, either '.el' or '.org' file."
  (let ((init-el (concat user-emacs-directory "ale.el"))
        (init-org (concat ale/init-dot-repo "ale.org")))
    (if (file-exists-p init-el)
        (load-file init-el)
      (require 'ob-tangle)
      (org-babel-tangle-file init-org init-el)
      (load-file init-el)
      (if (native-comp-available-p)
          (native-compile init-el)
        (byte-compile-file init-el)))))

(ale/init-load-config)

