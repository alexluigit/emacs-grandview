(require 'cl-lib)
(when (featurep 'embark)
  (require 'embark))

(defgroup ace/embark ()
  "Extensions for `embark'."
  :group 'editing)

(declare-function embark--act "embark")
(declare-function embark--target "embark")
(autoload 'embark-default-action "embark")

(defun ace/embark--completions-act (arg)
  "Move ARG lines and perform `embark-default-action'."
  (forward-line arg)
  (embark--act #'embark-default-action (cdr (embark--target))))

;;;###autoload
(defun ace/embark-completions-act-next (&optional arg)
  "Run default action on next or ARGth Embark target.
This calls `ace/embark--completions-act' and is meant to be
assigned to a key in `embark-collect-mode-map'."
  (interactive "p")
  (ace/embark--completions-act (or arg 1)))

;;;###autoload
(defun ace/embark-completions-act-previous (&optional arg)
  "Run default action on previous or ARGth Embark target.
This calls `ace/embark--completions-act' and is meant to be
assigned to a key in `embark-collect-mode-map'."
  (interactive "p")
  (let ((num (if (numberp arg) (- 0 arg))))
    (ace/embark--completions-act (or num -1))))

;;;###autoload
(defun ace/embark-collection-kill-line ()
  "Delete line from Embark collect buffer."
  (interactive)
  (let* ((inhibit-read-only t)
         (eol (point-at-eol))
         (eol-dwim (if (= eol (point-max)) eol (1+ eol))))
    (save-excursion
      (goto-char (point-at-bol))
      (delete-region (point) eol-dwim))))

(declare-function embark-consult-preview-minor-mode "embark-consult")
(defvar embark-consult-preview-minor-mode)

;;;###autoload
(defun ace/embark-consult-preview-toggle ()
  "Toggle preview mode for Embark's Consult collections."
  (interactive)
  (when (featurep 'embark-consult)
    (require 'embark-consult)
    (if (and (bound-and-true-p embark-consult-preview-minor-mode)
             (derived-mode-p 'embark-collect-mode))
        (progn
          (remove-hook 'embark-collect-mode-hook #'embark-consult-preview-minor-mode)
          (embark-consult-preview-minor-mode -1))
      (add-hook 'embark-collect-mode-hook #'embark-consult-preview-minor-mode)
      (embark-consult-preview-minor-mode 1))))

(autoload 'ace/project-fd-other-window "ace-project")
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
;; (autoload 'ace/recentf-recent-files "ace/recentf")
(autoload 'project-switch-to-buffer "project")
(autoload 'project-find-file "project")

(defvar ace/embark-extras-become-file+buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map embark-become-file+buffer-map)
    ;; (define-key map (kbd "r") 'ace/recentf-recent-files)
    (define-key map (kbd "B") 'project-switch-to-buffer)
    (define-key map (kbd "o") 'ace/project-fd-other-window)
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
  (let ((maps (list 'ace/embark-extras-become-general-map
                    'ace/embark-extras-become-line-map
                    'ace/embark-extras-become-file+buffer-map)))
    (if ace/embark-extras-keymaps
        (dolist (map maps)
          (cl-pushnew map embark-become-keymaps))
      (setq embark-become-keymaps
            (dolist (map maps)
              (delete map embark-become-keymaps))))))

(provide 'ace-embark)
