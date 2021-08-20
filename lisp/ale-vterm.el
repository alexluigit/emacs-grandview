;;; ale-vterm.el --- -*- lexical-binding: t -*-

(defcustom ale/vterm-position
  '((danger-mode . ((window-height . 0.4) (side . bottom)))
    (default . ((window-width . 0.4) (side . right))))
  "doc")

(defvar ale/vterm-buffers nil
  "The list of non-dedicated vterm buffers.")

(defvar ale/vterm-index 0
  "The index of current vterm buffer.")

(add-hook 'kill-buffer-hook
          (lambda ()
            (let* ((buf (current-buffer))
                   (name (buffer-name buf)))
              (when (string-prefix-p "*vterm" name)
                (delq! buf ale/vterm-buffers)))))

;;;###autoload
(defun ale/vterm--disable-side-window (fn &rest args)
  "Prevent vterm size adjust break selection."
  (unless (and (region-active-p)
               (derived-mode-p 'vterm-mode))
    (apply fn args)))

;;;###autoload
(advice-add 'display-buffer-in-side-window :around 'ale/vterm--disable-side-window)

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
(defun vterm-send-F5 ()
  (interactive)
  (vterm-send-key "<f5>" nil nil nil))

(defun ale/vterm--get-win-params ()
  "Parse `ale/vterm-position' to get vterm display parameters."
  (let (pos)
    (cl-dolist (setting ale/vterm-position)
        (when (derived-mode-p (car setting))
          (setq pos (cdr setting)) (cl-return))
        (when (eq (car setting) 'default)
          (setq pos (cdr setting))))
    `(("^\\*vterm.*"
       (display-buffer-in-side-window)
       (window-parameters . ((mode-line-format . none)))
       ,@pos))))

;;;###autoload
(defun ale/vterm-toggle (&optional force-new)
  "Toggle vterm.
If called with prefix argument, create a new vterm buffer if
current one have different `default-directory'."
  (interactive "P")
  (if (eq major-mode 'vterm-mode)
      (delete-window)
    (let* ((display-buffer-alist (ale/vterm--get-win-params))
           (buf (nth ale/vterm-index ale/vterm-buffers))
           (dir (expand-file-name default-directory))
           (index (if buf (ale/vterm--get-index buf) 0)))
      (add-to-list 'ale/vterm-buffers (vterm index))
      (when force-new
        (unless (string= dir (expand-file-name default-directory))
          (let ((default-directory dir))
            (ale/vterm-new))))
      (ale/vterm--insert))))

(defun ale/vterm--get-index (buf)
  (let* ((name (buffer-name buf)))
    (string-match "\\*vterm\\*\<\\([0-9]+\\)\>" name)
    (string-to-number (cl-subseq name (match-beginning 1) (match-end 1)))))

(defun ale/vterm--insert ()
  (when (featurep 'evil) (evil-insert-state))
  (when (featurep 'meow) (meow-insert)))

;;;###autoload
(defun ale/vterm-new ()
  "Create new vterm buffer."
  (interactive)
  (let ((new-index (1+ (ale/vterm--get-index (car ale/vterm-buffers))))
        (display-buffer-alist (ale/vterm--get-win-params)))
    (add-to-list 'ale/vterm-buffers (vterm new-index))
    (ale/vterm--insert)))

;;;###autoload
(defun ale/vterm-next (&optional arg)
  "Select next vterm buffer.
Create new one if no vterm buffer exists."
  (interactive "P")
  (let* ((curr-index (cl-position (current-buffer) ale/vterm-buffers))
         (new-index (+ curr-index (or arg -1)))
         (buf (nth new-index ale/vterm-buffers)))
    (when buf
      (switch-to-buffer buf)
      (setq ale/vterm-index new-index))))

;;;###autoload
(defun ale/vterm-prev (&optional arg)
  "Select previous vterm buffer."
  (interactive "p")
  (ale/vterm-next arg))

(provide 'ale-vterm)
