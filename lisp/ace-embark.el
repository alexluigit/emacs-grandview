(require 'cl-lib)
(when (featurep 'embark)
  (require 'embark))

(defgroup ace/embark ()
  "Extensions for `embark'."
  :group 'editing)

(autoload 'consult-grep "consult")
(autoload 'consult-line "consult")
(autoload 'consult-imenu "consult")
(autoload 'consult-outline "consult")

(defvar ace/embark-extras-become-general-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'consult-grep)
    map)
  "General custom cross-package `embark-become' keymap.")

(defvar ace/embark-extras-become-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") 'consult-line)
    (define-key map (kbd "i") 'consult-imenu)
    (define-key map (kbd "s") 'consult-outline) ; as my default is 'M-s s'
    map)
  "Line-specific custom cross-package `embark-become' keymap.")

(defvar embark-become-file+buffer-map)
(autoload 'project-switch-to-buffer "project")
(autoload 'project-find-file "project")

(defvar ace/embark-extras-become-file+buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map embark-become-file+buffer-map)
    (define-key map (kbd "B") 'project-switch-to-buffer)
    (define-key map (kbd "F") 'project-find-file)
    map)
  "File+buffer custom cross-package `embark-become' keymap.")

(defvar embark-become-keymaps)

;;;###autoload
(define-minor-mode ace/embark-extras-keymaps
  "Add or remove keymaps from Embark.
This is based on the value of `ace/embark-extras-add-keymaps'
and is meant to keep things clean in case I ever wish to disable
those so-called 'extras'."
  :init-value nil
  :global t
  (let ((maps '(ace/embark-extras-become-general-map
                ace/embark-extras-become-line-map
                ace/embark-extras-become-file+buffer-map)))
    (if ace/embark-extras-keymaps
        (dolist (map maps)
          (cl-pushnew map embark-become-keymaps))
      (setq embark-become-keymaps
            (dolist (map maps)
              (delete map embark-become-keymaps))))))

(provide 'ace-embark)
