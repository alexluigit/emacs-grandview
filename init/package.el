;;; init/packages.el --- -*- lexical-binding: t -*-

(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)
(setq straight-check-for-modifications '(check-on-save find-when-checking))

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq use-package-always-defer t)

(defvar ale--deferred-packages-alist '(t))

(with-eval-after-load 'use-package-core
  (push :after-call use-package-deferring-keywords)
  (setq use-package-keywords
        (use-package-list-insert :after-call use-package-keywords :after))
  (defalias 'use-package-normalize/:after-call #'use-package-normalize-symlist)
  (defun use-package-handler/:after-call (name _keyword hooks rest state)
    "Adds keyword `:after-call' to `use-package' to expand its lazy-loading capabilities."
    (if (plist-get state :demand)
        (use-package-process-keywords name rest state)
      (let ((fn (make-symbol (format "ale--after-call-%s-h" name))))
        (use-package-concat
         `((fset ',fn
                 (lambda (&rest _)
                   (ale-log "Loading deferred package %s from %s" ',name ',fn)
                   (condition-case e
                       (let ((default-directory user-emacs-directory))
                         (require ',name))
                     ((debug error)
                      (message "Failed to load deferred package %s: %s" ',name e)))
                   (when-let (deferral-list (assq ',name ale--deferred-packages-alist))
                     (dolist (hook (cdr deferral-list))
                       (advice-remove hook #',fn)
                       (remove-hook hook #',fn))
                     (setq ale--deferred-packages-alist
                           (delq deferral-list ale--deferred-packages-alist))
                     (unintern ',fn nil)))))
         (let (forms)
           (dolist (hook hooks forms)
             (push (if (string-match-p "-\\(?:functions\\|hook\\)$" (symbol-name hook))
                       `(add-hook ',hook #',fn)
                     `(advice-add #',hook :before #',fn))
                   forms)))
         `((unless (assq ',name ale--deferred-packages-alist)
             (push '(,name) ale--deferred-packages-alist))
           (nconc (assq ',name ale--deferred-packages-alist)
                  '(,@hooks)))
         (use-package-process-keywords name rest state))))))
