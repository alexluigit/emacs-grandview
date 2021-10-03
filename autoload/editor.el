;;; autoload/editor.el --- -*- lexical-binding: t -*-

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

;;;###autoload
(defadvice keyboard-escape-quit (around keep-windows activate)
  "Do not close any window when calling `keyboard-escape-quit'."
  (let ((buffer-quit-function #'ignore)) ad-do-it))

;;;###autoload
(defadvice next-error-no-select (around reuse-window activate)
  "Do not open new window when calling `next-error-no-select'."
  (let ((split-width-threshold nil)) ad-do-it))

;;;###autoload
(defadvice previous-error-no-select (around reuse-window activate)
  "Do not open new window when calling `previous-error-no-select'."
  (let ((split-width-threshold nil)) ad-do-it))

;;;###autoload
(defadvice yank (before bol activate)
  "Make `yank' behave like paste (p) command in vim."
  (when-let* ((clip (condition-case nil (current-kill 0 t) (error "")))
              (remove-syntax (set-text-properties 0 (length clip) nil clip))
              (end-with-newline (string-suffix-p "\n" clip)))
    (goto-char (line-beginning-position))))

;;;###autoload
(defadvice delete-backward-char (around no-read-only activate)
  "Do not try to delete char when last char is read-only."
  (unless (get-text-property (1- (point)) 'read-only) ad-do-it))

;;;###autoload
(defun ale-next-line (&optional arg)
  "Move to the next line.

Will cancel all other selection, except char selection.  Use with
numeric argument to move multiple lines at once."
  (interactive "p")
  (unless (equal (meow--selection-type) '(expand . char))
    (meow--cancel-selection))
  (call-interactively 'next-line))

;;;###autoload
(defun ale-prev-line (&optional arg)
  "See `ale-next-line'."
  (interactive "p")
  (unless (equal (meow--selection-type) '(expand . char))
    (meow--cancel-selection))
  (call-interactively 'previous-line))

;;;###autoload
(defun ale-save ()
  (interactive)
  (save-excursion
    (meow--with-selection-fallback
     (meow--prepare-region-for-kill)
     (call-interactively 'kill-ring-save))))

;;;###autoload
(defun ale-backward-delete-char ()
  (interactive)
  (when (and (not (ale--char-readonly))
             (> (point) (point-at-bol)))
    (delete-backward-char 1)))

;;;###autoload
(defun ale-insert-ctrl-i ()
  (interactive)
  (cond ((derived-mode-p 'vterm-mode)
         (vterm-send-C-i))
        (t
         (call-interactively 'forward-char))))

;;;###autoload
(defun ale-insert-ctrl-o ()
  (interactive)
  (cond ((derived-mode-p 'vterm-mode)
         (vterm-send-C-o))
        (t
         (call-interactively 'backward-char))))

;;;###autoload
(defun ale-kill-whole-line ()
  (interactive)
  (cond ((derived-mode-p 'eshell-mode)
         (eshell-kill-input))
        ((derived-mode-p 'vterm-mode)
         (vterm-send-C-u))
        (t
         (call-interactively 'kill-whole-line))))

;;;###autoload
(defun ale-top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

;;;###autoload
(defun ale-inner-line ()
  "Mark inner line and move cursor to bol."
  (interactive)
  (save-window-excursion
    (end-of-visual-line)
    (set-mark-command nil)
    (back-to-indentation)))

;;;###autoload
(defun ale-comment-or-uncomment-region ()
  "Toggle comment in active-region (when available) or current line."
  (interactive)
  (let ((comment-func (if (eq major-mode 'org-mode)
                          'org-comment-dwim
                        'comment-or-uncomment-region)))
    (unless (region-active-p) (meow-line 1))
    (call-interactively comment-func)))

;;;###autoload
(defun ale-query-replace (&optional arg)
  (interactive "P")
  (if arg
      (progn
        (run-with-timer 0.05 nil 'yank)
        (anzu-query-replace nil))
    (if (region-active-p)
        (progn
          (call-interactively 'kill-ring-save)
          (exchange-point-and-mark)
          (deactivate-mark t)
          (run-with-timer 0.05 nil 'yank)
          (anzu-query-replace-regexp nil))
      (anzu-query-replace-at-cursor))))

;;;###autoload
(defun ale-ispell-word ()
  "Complete the symbol at point based on entries in the dictionary.

This function will generate a bunch of relevent (corrently spelled)
word by looking up all the entries in the dictionary accoording to the
last partial word user have typed, then let user to choose the one
they want by in minibuffer, finally replace word at point with the
selected one. "
  (interactive)
  (autoload 'ispell-lookup-words "ispell")
  (when-let* ((word (thing-at-point 'symbol t))
              (boundaries (bounds-of-thing-at-point 'symbol))
              (start (car boundaries))
              (end (cdr boundaries))
              (words (ispell-lookup-words word))
              (selection (completing-read "Words: " words)))
    (delete-region start end) (insert selection)))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun ale-insert ()
  (interactive)
  (meow--switch-state 'insert))

;;;###autoload
(defun ale-insert-at-first-non-whitespace ()
  (interactive)
  (back-to-indentation)
  (meow-insert))

;;;###autoload
(defun ale-escape ()
  (interactive)
  (cond
   ((derived-mode-p 'vterm-mode) (vterm-send-escape))
   ((region-active-p) (meow-cancel))
   ((meow-keypad-mode-p)
    (meow--exit-keypad-state))
   ((meow-insert-mode-p)
    (when overwrite-mode (overwrite-mode -1))
    (meow--switch-state 'normal))
   (t (call-interactively 'execute-extended-command))))

;;;###autoload
(defun ale--bounds-of-tag ()
  (meow--bounds-of-regexp "<.*>"))

;;;###autoload
(defun ale--inner-of-tag ()
  (when-let ((pos (ale--bounds-of-tag)))
    (save-mark-and-excursion
      (let (ibeg iend)
        (goto-char (car pos))
        (setq ibeg (search-forward ">"))
        (goto-char (cdr pos))
        (setq iend (search-backward "<"))
        (cons ibeg iend)))))

;;;###autoload
(defun ale-electric-inhibit-< ()
  (setq-local electric-pair-inhibit-predicate
              `(lambda (c) (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c)))))

(defvar ale-jumper-cmd-alist
  '(beginning-of-buffer
    end-of-buffer
    forward-sexp
    backward-sexp
    meow-next
    meow-prev
    meow-search
    consult-outline
    consult-line
    consult-project-imenu
    avy-goto-char-timer
    er/expand-region
    xref-find-definitions)
  "A list of file, adviced function, and advice function.")

(defun ale-jumper-advice (fn &rest args)
  (let ((old-pos (point)))
    (apply fn args)
    (when (> (abs (- (line-number-at-pos old-pos) (line-number-at-pos (point)))) 1)
      (better-jumper-set-jump old-pos))))

;;;###autoload
(define-minor-mode ale-jumper-sensible-jump-mode
  "Add sensible commands to jump list."
  :init-value nil
  :global t
  (if ale-jumper-sensible-jump-mode
      (dolist (sym ale-jumper-cmd-alist)
        (advice-add sym :around 'ale-jumper-advice))
    (dolist (sym ale-jumper-cmd-alist)
      (advice-remove sym 'ale-jumper-advice))))
