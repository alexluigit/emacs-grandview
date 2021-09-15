;;; autoload/embark.el --- -*- lexical-binding: t -*-

(autoload 'consult-grep "consult")
(autoload 'consult-line "consult")
(autoload 'consult-imenu "consult-imenu")
(autoload 'consult-outline "consult")

(defvar ale-embark-become-general-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'consult-grep)
    map)
  "General custom cross-package `embark-become' keymap.")

(defvar ale-embark-become-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") 'consult-line)
    (define-key map (kbd "i") 'consult-imenu)
    (define-key map (kbd "s") 'consult-outline) ; as my default is 'M-s s'
    map)
  "Line-specific custom cross-package `embark-become' keymap.")

(defvar embark-become-file+buffer-map)
(autoload 'project-switch-to-buffer "project")
(autoload 'project-find-file "project")

(defvar ale-embark-become-file+buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map embark-become-file+buffer-map)
    (define-key map (kbd "B") 'project-switch-to-buffer)
    (define-key map (kbd "F") 'project-find-file)
    map)
  "File+buffer custom cross-package `embark-become' keymap.")

(defvar embark-become-keymaps)

;;;###autoload
(define-minor-mode ale-embark-keymaps
  "Add or remove keymaps from Embark.
This is based on the value of `ale-embark-add-keymaps'
and is meant to keep things clean in case I ever wish to disable
those so-called 'extras'."
  :init-value nil
  :global t
  (let ((maps '(ale-embark-become-general-map
                ale-embark-become-line-map
                ale-embark-become-file+buffer-map)))
    (if ale-embark-keymaps
        (dolist (map maps)
          (cl-pushnew map embark-become-keymaps))
      (setq embark-become-keymaps
            (dolist (map maps)
              (delete map embark-become-keymaps))))))
