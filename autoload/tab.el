;;; ale-tab.el -*- lexical-binding: t; -*-

(require 'tab-bar)

(defun ale-tab--tab-bar-tabs ()
  "Return a list of `tab-bar' tabs, minus the current one."
  (mapcar (lambda (tab)
            (alist-get 'name tab))
          (tab-bar--tabs-recent)))

;;;###autoload
(defun ale-tab-select-tab-dwim ()
    "Do-What-I-Mean function for getting to a `tab-bar' tab.
If no other tab exists, create one and switch to it.  If there is
one other tab (so two in total) switch to it without further
questions.  Else use completion to select the tab to switch to."
    (interactive)
    (let ((tabs (ale-tab--tab-bar-tabs)))
      (cond ((null tabs)
             (tab-new))
            ((eq (length tabs) 1)
             (tab-next))
            (t
             (tab-bar-switch-to-tab
              (completing-read "Select tab: " tabs nil t))))))

;;;###autoload
(defun ale-tab-tab-bar-toggle ()
  "Toggle `tab-bar' presentation."
  (interactive)
  (if (bound-and-true-p tab-bar-mode)
      (progn
        (setq tab-bar-show nil)
        (tab-bar-mode -1))
    (setq tab-bar-show t)
    (tab-bar-mode 1)))

(cl-loop for i in '(1 2 3 4 5 6 7 8 9) do
         (global-set-key (kbd (format "M-%s" i)) 'awesome-tab-select-visible-tab))

(defvar ale-tab-hide-regexp
  '("^\\*\\(message\\|straight\\|ibuffer\\|epc\\|async-native\\)"
    "^\\*\\(compile-Log\\|danger\\|eshell\\|vterm\\|ediff\\|help\\)"
    "^ \\*" "^magit.*"))

;;;###autoload
(defun ale-tab-hide-tab (x)
  (let ((name (format "%s" x))
        (hide-regex (mapconcat 'concat ale-tab-hide-regexp "\\|")))
    (string-match hide-regex name)))

;;;###autoload
(defun ale-tab-buffer-groups ()
  "`awesome-tab-buffer-groups' control buffers' group rules."
  (list
   (cond
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Emacs")
    ((memq major-mode '(magit-process-mode
                        magit-status-mode
                        magit-diff-mode
                        magit-log-mode
                        magit-file-mode
                        magit-blob-mode
                        magit-blame-mode))
     "Magit")
    ((derived-mode-p 'emacs-lisp-mode)
     "Elisp")
    ((memq major-mode '(org-mode org-agenda-mode diary-mode))
     "OrgMode")
    (t
     (awesome-tab-get-group-name (current-buffer))))))
