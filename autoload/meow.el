;;; autoload/meow.el --- -*- lexical-binding: t -*-

(require 'meow)

;;;###autoload
(defun ale-meow-save ()
  (interactive)
  (save-excursion
    (meow--with-selection-fallback
     (meow--prepare-region-for-kill)
     (call-interactively 'kill-ring-save))))

;;;###autoload
(defun ale-meow-query-replace-auto-fill-advisor (fn &rest args)
  "Call `meow-query-replace' and auto fill prompt with region text."
  (unless (region-active-p) (meow-mark-symbol 1))
  (call-interactively 'kill-ring-save)
  (exchange-point-and-mark)
  (deactivate-mark t)
  (run-with-timer 0.05 nil 'yank)
  (apply fn args))

;;;###autoload
(defun ale-meow-escape ()
  (interactive)
  (cond
   ((region-active-p) (meow-cancel))
   (t (call-interactively 'execute-extended-command))))

;;;###autoload
(defun ale-meow-leader-space ()
  "If `SPC' is bounded to any command in current major-mode, call
that command, otherwise call `consult-buffer'."
  (interactive)
  (let ((key (meow--parse-input-event last-input-event)))
    (if-let* ((rebind-key (meow--get-origin-command key)))
        (meow--execute-kbd-macro rebind-key)
      (consult-buffer))))

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
(defun ale-meow-insert ()
  (interactive)
  (meow--switch-state 'insert))

;;;###autoload
(defun ale-meow-insert-at-first-non-whitespace ()
  (interactive)
  (back-to-indentation)
  (meow-insert))

(provide 'ale-meow)
