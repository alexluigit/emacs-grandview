;;; init/editor.el --- -*- lexical-binding: t -*-

(defcustom ale-date-specifier "%F"
  "Date specifier for `format-time-string'.
Used by `ale-insert-date'."
  :type 'string
  :group 'ale)

(defcustom ale-time-specifier "%R %z"
  "Time specifier for `format-time-string'.
Used by `ale-insert-date'."
  :type 'string
  :group 'ale)

(defcustom ale-quit-minor-modes '(org-tree-slide-mode)
  "Disable these minor modes when calling `ale-quit'."
  :type '(repeat symbol)
  :group 'ale)

(defadvice keyboard-escape-quit (around keep-windows activate)
  "Do not close any window when calling `keyboard-escape-quit'."
  (let ((buffer-quit-function #'ignore)) ad-do-it))

(defadvice next-error-no-select (around reuse-window activate)
  "Do not open new window when calling `next-error-no-select'."
  (let ((split-width-threshold nil)) ad-do-it))

(defadvice previous-error-no-select (around reuse-window activate)
  "Do not open new window when calling `previous-error-no-select'."
  (let ((split-width-threshold nil)) ad-do-it))

(defadvice yank (before bol activate)
  "Make `yank' behave like paste (p) command in vim."
  (when-let ((clip (condition-case nil (current-kill 0 t) (error ""))))
    (set-text-properties 0 (length clip) nil clip)
    (when (string-suffix-p "\n" clip)
      (goto-char (line-beginning-position)))))

(defadvice delete-backward-char (around no-read-only activate)
  "Do not try to delete char when last char is read-only."
  (unless (get-text-property (1- (point)) 'read-only) ad-do-it))

(defun ale-top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

(defun ale-inner-line ()
  "Mark inner line and move cursor to bol."
  (interactive)
  (save-window-excursion
    (end-of-visual-line)
    (set-mark-command nil)
    (back-to-indentation)))

(defun ale-match-paren (arg)
  "Go to the matching paren if on a paren; otherwise
forward-list (backward-list if `arg' is Non-nil).
This command emulates the `%' key in vim."
  (interactive "p")
  (cond
   ((char-equal 41 (char-before)) (backward-list 1))
   ((char-equal 125 (char-before)) (backward-list 1))
   ((and
     (char-equal 123 (char-before))
     (char-equal 10 (char-after)))
    (backward-char 1) (forward-list 1))
   ((looking-at "\\s\(") (forward-list 1))
   ((looking-at "\\s\)") (backward-list 1))
   (t (if arg (backward-list 1) (forward-list 1)))))

(defun ale-comment-or-uncomment-region ()
  "Toggle comment in active-region (when available) or current line."
  (interactive)
  (let ((comment-func (if (eq major-mode 'org-mode)
                          'org-comment-dwim
                        'comment-or-uncomment-region))
        (old-point (point)))
    (unless (region-active-p)
      (goto-char (line-beginning-position))
      (set-mark-command nil)
      (forward-line 1))
    (call-interactively comment-func)
    (goto-char old-point)))

(defun ale-insert-date (&optional arg)
  "Insert the current date as `ale-date-specifier'.

With optional prefix ARG (\\[universal-argument]) also append the
current time understood as `ale-time-specifier'.

When region is active, delete the highlighted text and replace it
with the specified date."
  (interactive "P")
  (let* ((date ale-date-specifier)
         (time ale-time-specifier)
         (format (if arg (format "%s %s" date time) date)))
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (insert (format-time-string format))))

(autoload 'ffap-url-at-point "ffap")
(defvar ffap-string-at-point-region)

(defun ale-rename-file-and-buffer (name)
  "Apply NAME to current file and rename its buffer.
Do not try to make a new directory or anything fancy."
  (interactive
   (list (read-string "Rename current file: " (buffer-file-name))))
  (let* ((file (buffer-file-name)))
    (if (vc-registered file)
        (vc-rename-file file name)
      (rename-file file name))
    (set-visited-file-name name t t)))

(defun ale-quit ()
  "Disable some minor modes or kill current window/buffer.

Try to quit minor modes defined in `ale-quit-minor-modes', if
none of these minor modes were enabled, try to delete window, if
any error occurs, kill this buffer instead."
  (interactive)
  (let ((skip-kill-window nil))
    (dolist (mode ale-quit-minor-modes)
      (when (and (boundp mode)
                 (or (derived-mode-p mode)
                     (buffer-local-value mode (current-buffer))))
        (setq skip-kill-window t)
        (apply mode '(-1))))
    (unless skip-kill-window
      (condition-case nil
          (delete-window)
        (error (kill-this-buffer))))))
