(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(defvar ace/emacs-init-org-path "/home/alex/Dev/alex.files/config/emacs/ace.org"
  "Main configuration org file path")

(defvar ace/emacs-autoinstall-elpa nil
  "Whether `ace/elpa-package' should install packages.
The default nil value means never to automatically install
packages.  A non-nil value is always interpreted as consent for
auto-installing everything---this process does not cover manually
maintained git repos, controlled by `ace/manual-package'.")

(defvar ace/emacs-basic-init "basic-init.el"
  "Name of 'basic init' file.

This file is meant to store user configurations that are evaluated
before loading `ace/emacs-configuration-main-file' and, when
available, `ace/emacs-configuration-user-file'.  Those values
control the behaviour of the Emacs setup.

The only variable that is currently expected to be in the 'basic
init' file is `ace/emacs-autoinstall-elpa'.

See `ace/emacs-basic-init-setup' for the actual initialisation
process.")

(defvar ace/emacs-configuration-main-file "ace"
  "Base name of the main configuration file.")

(defvar ace/emacs-configuration-user-file "user"
  "Base name of user-specific configuration file.")

(defvar ace/package-list nil "List of package names to install, if missing.")


(defun ace/emacs--expand-file-name (file extension)
  "Return canonical path to FILE with EXTENSION."
  (expand-file-name
   (concat user-emacs-directory file extension)))

(defun ace/emacs-basic-init-setup ()
  "Load 'basic-init.el' if it exists.
This is meant to evaluate forms that control the rest of my Emacs
setup."
  (let* ((init ace/emacs-basic-init)
         (file (thread-last user-emacs-directory (expand-file-name init))))
    (when (file-exists-p file)
      (load-file file))))

(defun ace/ensure-install ()
  "Install all `ace/package-list' packages, if needed."
  (interactive)
  (when (yes-or-no-p (format "Try to install %d packages?"
                             (length ace/package-list)))
  (package-refresh-contents)
  (mapc (lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
        ace/package-list)))

(defmacro ace/builtin-package (package &rest body)
  "Set up builtin PACKAGE with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions."
  (declare (indent 1))
  `(progn
     (unless (require ,package nil 'noerror)
       (display-warning 'ace-init (format "Loading `%s' failed" ,package) :warning))
     ,@body))

(defmacro ace/elpa-package (package &rest body)
  "Set up PACKAGE from an Elisp archive with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions.

When `ace/autoinstall-elpa' is non-nil try to install the
package if it is missing."
  `(progn
     (when (and ace/emacs-autoinstall-elpa
                (not (package-installed-p ,package)))
       (package-install ,package))
     (if (require ,package nil 'noerror)
         (progn ,@body)
       (display-warning 'ace-init (format "Loading `%s' failed" ,package) :warning)
       (add-to-list 'ace/package-list ,package)
       (display-warning 'ace-init
        (format "Run `ace/install-ensured' to install all packages in `ace/package-list'")
        :warning))))

(defmacro ace/manual-package (package &rest body)
  "Set up manually installed PACKAGE with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions."
  (declare (indent 1))
  (let ((path (thread-last user-emacs-directory
                (expand-file-name "contrib")
                (expand-file-name (symbol-name (eval package))))))
    `(progn
       (eval-and-compile
         (add-to-list 'load-path ,path))
       (if (require ,package nil 'noerror)
         (with-eval-after-load ,package ,@body)
         (display-warning 'ace-init (format "Loading `%s' failed" ,package) :warning)
         (display-warning 'ace-init (format "This must be available at %s" ,path) :warning)))))

(add-to-list 'load-path (concat user-emacs-directory "ace")) ; custom libraries
(let ((default-directory (concat user-emacs-directory "contrib/")))
  (normal-top-level-add-subdirs-to-load-path))

(defun ace/emacs-load-config ()
  "Load main Emacs configurations, either '.el' or '.org' file."
  (let* ((main-init ace/emacs-configuration-main-file)
         (main-init-el (ace/emacs--expand-file-name main-init ".el"))
         (main-init-org (ace/emacs--expand-file-name main-init ".org"))
         (user-init ace/emacs-configuration-user-file)
         (user-init-el (ace/emacs--expand-file-name user-init ".el"))
         (user-init-org (ace/emacs--expand-file-name user-init ".org")))
    (ace/emacs-basic-init-setup)
    (require 'org)
    (if (file-exists-p main-init-el)
        (load-file main-init-el)
      (when (file-exists-p main-init-org)
        (org-babel-load-file main-init-org)))
    (if (file-exists-p user-init-el)
        (load-file user-init-el)
      (when (file-exists-p user-init-org)
        (org-babel-load-file user-init-org)))))

;; Load configurations.
(ace/emacs-load-config)

(defun ace/emacs-build-config ()
  "Automatically tangle main init org file at saving when current buffer name matches.
Add this to `after-save-hook' in `org-mode-hook'."
  (unless (string= buffer-file-name ace/emacs-init-org-path) (error "Main init untangled"))
  (let* ((main-init ace/emacs-configuration-main-file)
         (main-init-el (ace/emacs--expand-file-name main-init ".el"))
         (main-init-org (ace/emacs--expand-file-name main-init ".org"))
         (user-init ace/emacs-configuration-user-file)
         (user-init-el (ace/emacs--expand-file-name user-init ".el"))
         (user-init-org (ace/emacs--expand-file-name user-init ".org")))
    (when (file-exists-p main-init-el)
      (delete-file main-init-el))
    (when (file-exists-p user-init-el)
      (delete-file user-init-el))
    (require 'org)
    (when (file-exists-p main-init-org)
      (org-babel-tangle-file main-init-org main-init-el))
    (when (file-exists-p user-init-org)
      (org-babel-tangle-file user-init-org user-init-el))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'ace/emacs-build-config)))
