(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))
(require 'use-package)

(defvar ace/init-org-path "/home/alex/Dev/alex.files/config/emacs/ace.org"
  "Main configuration org file path")

(defvar ace/init-el-path "ace"
  "Base name of the main configuration file.")

(defun ace/init--expand-file-name (file extension)
  "Return canonical path to FILE with EXTENSION."
  (expand-file-name
   (concat user-emacs-directory file extension)))

(add-to-list 'load-path (concat user-emacs-directory "ace")) ; custom libraries
(let ((default-directory (concat user-emacs-directory "contrib/")))
  (normal-top-level-add-subdirs-to-load-path))

(defun ace/init-load-config ()
  "Load main Emacs configurations, either '.el' or '.org' file."
  (let* ((main-init ace/init-el-path)
         (main-init-el (ace/init--expand-file-name main-init ".el"))
         (main-init-org (ace/init--expand-file-name main-init ".org")))
    (require 'org)
    (if (file-exists-p main-init-el)
        (load-file main-init-el)
      (when (file-exists-p main-init-org)
        (org-babel-load-file main-init-org)))))

;; Load configurations.
(ace/init-load-config)

(defun ace/init-build-config ()
  "Automatically tangle main init org file at saving when current buffer name matches.
Add this to `after-save-hook' in `org-mode-hook'."
  (unless (string= buffer-file-name ace/init-org-path) (error "Main init untangled"))
  (let* ((main-init ace/init-el-path)
         (main-init-el (ace/init--expand-file-name main-init ".el"))
         (main-init-org (ace/init--expand-file-name main-init ".org")))
    (when (file-exists-p main-init-el)
      (delete-file main-init-el))
    (require 'org)
    (when (file-exists-p main-init-org)
      (org-babel-tangle-file main-init-org main-init-el))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'ace/init-build-config)))
