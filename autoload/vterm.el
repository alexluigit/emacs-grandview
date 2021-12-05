;;; ale-vterm.el --- -*- lexical-binding: t -*-

(require 'vterm)

(defcustom ale-vterm-kill-whole-line-cmd
  'vterm-send-C-u
  "Command for kill whole line in vterm buffer."
  :type 'symbol :group 'vterm)

(defcustom ale-vterm-position
  '((dirvish-mode . ((window-height . 0.4) (side . bottom)))
    (default . ((window-width . 0.4) (side . right))))
  "doc")

(defvar ale-vterm-buffers nil
  "The list of non-dedicated vterm buffers.")

(defvar ale-vterm-index 0
  "The index of current vterm buffer.")

;;;###autoload
(defun ale-vterm-escape-advisor (fn &rest args)
  (if (derived-mode-p 'vterm-mode)
      (vterm-send-escape)
    (apply fn args)))

;;;###autoload
(defun ale-vterm-kill-whole-line-advisor (fn &rest args)
  (if (derived-mode-p 'vterm-mode)
      (funcall ale-vterm-kill-whole-line-cmd)
    (apply fn args)))

(defun ale-vterm--disable-side-window (fn &rest args)
  "Prevent vterm size adjust break selection."
  (unless (and (region-active-p)
               (derived-mode-p 'vterm-mode))
    (apply fn args)))

;;;###autoload
(defun vterm-send-C-delete ()
  (interactive)
  (vterm-send-key "<delete>" nil nil 0))

;;;###autoload
(defun vterm-send-M-return ()
  (interactive)
  (vterm-send-escape)
  (vterm-send-return))

;;;###autoload
(defun vterm-send-M-/ ()
  (interactive)
  (vterm-send-key "/" nil 0 nil))

;;;###autoload
(defun vterm-send-F5 ()
  (interactive)
  (vterm-send-key "<f5>" nil nil nil))

;;;###autoload
(defun vterm-send-M-apostrophe ()
  (interactive)
  (vterm-send-key "'" nil 0 nil))

;;;###autoload
(defun vterm-send-M-quote ()
  (interactive)
  (vterm-send-key "\"" nil 0 nil))

(defun ale-vterm--get-win-params ()
  "Parse `ale-vterm-position' to get vterm display parameters."
  (let (pos)
    (cl-dolist (setting ale-vterm-position)
      (when (derived-mode-p (car setting))
        (setq pos (cdr setting)) (cl-return))
      (when (eq (car setting) 'default)
        (setq pos (cdr setting))))
    `(("^\\*vterm.*"
       (display-buffer-in-side-window)
       (window-parameters . ((mode-line-format . none)))
       ,@pos))))

;;;###autoload
(defun ale-vterm-toggle (&optional project-root)
  "Toggle vterm.
If called with prefix argument, create a new vterm buffer with
project root directory as `default-directory'."
  (interactive "P")
  (if (eq major-mode 'vterm-mode)
      (delete-window)
    (let* ((display-buffer-alist (ale-vterm--get-win-params))
           (buf (nth ale-vterm-index ale-vterm-buffers))
           (pr-root (or (cdr-safe (project-current)) default-directory))
           (default-directory (if project-root pr-root default-directory))
           (index (if buf (ale-vterm--get-index buf) 0)))
      (add-to-list 'ale-vterm-buffers (vterm index))
      (ale-vterm--insert))))

(defun ale-vterm--get-index (buf)
  (let* ((name (buffer-name buf)))
    (string-match "\\*vterm\\*\<\\([0-9]+\\)\>" name)
    (string-to-number (cl-subseq name (match-beginning 1) (match-end 1)))))

(declare-function meow-insert "meow-command")
(declare-function evil-insert-state "evil")
(defun ale-vterm--insert ()
  (cond
   ((featurep 'meow)
    (meow-insert))
   ((featurep 'evil)
    (evil-insert-state))))

;;;###autoload
(defun ale-vterm-new ()
  "Create new vterm buffer."
  (interactive)
  (let ((new-index (1+ (ale-vterm--get-index (car ale-vterm-buffers))))
        (display-buffer-alist (ale-vterm--get-win-params)))
    (add-to-list 'ale-vterm-buffers (vterm new-index))
    (ale-vterm--insert)))

;;;###autoload
(defun ale-vterm-next (&optional arg)
  "Select next vterm buffer.
Create new one if no vterm buffer exists."
  (interactive "P")
  (let* ((curr-index (cl-position (current-buffer) ale-vterm-buffers))
         (new-index (+ curr-index (or arg -1)))
         (buf (nth new-index ale-vterm-buffers)))
    (when buf
      (switch-to-buffer buf)
      (setq ale-vterm-index new-index))))

;;;###autoload
(defun ale-vterm-prev (&optional arg)
  "Select previous vterm buffer."
  (interactive "p")
  (ale-vterm-next arg))

(defun ale-vterm--kill-buffer ()
  "Remove killed buffer from `ale-vterm-buffers'.

Used as a hook function added to `kill-buffer-hook'."
  (let* ((buf (current-buffer))
         (name (buffer-name buf)))
    (when (string-prefix-p "*vterm" name)
      (delq! buf ale-vterm-buffers))))

;;;###autoload
(define-minor-mode ale-vterm-mux-mode
  "Show/hide multiple vterm windows under control."
  :global t
  :group 'vterm
  (if ale-vterm-mux-mode
      (progn
        (advice-add 'display-buffer-in-side-window :around 'ale-vterm--disable-side-window)
        (add-hook 'kill-buffer-hook #'ale-vterm--kill-buffer))
    (advice-remove 'display-buffer-in-side-window 'ale-vterm--disable-side-window)
    (remove-hook 'kill-buffer-hook #'ale-vterm--kill-buffer)))

(provide 'ale-vterm)

;;; autoload/vterm.el ends here
