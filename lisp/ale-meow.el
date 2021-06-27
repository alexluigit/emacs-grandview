(require 'ale-files)
(require 'ale-simple)
(require 'ale-window)
(require 'dash)

(defun ale/meow--char-readonly ()
  (get-text-property (1- (point)) 'read-only))

(defun ale/meow-forward-char ()
  (interactive)
  (when (< (point) (point-at-eol))
    (forward-char)))

(defun ale/meow-backward-char ()
  (interactive)
  (when (and (not (ale/meow--char-readonly))
             (> (point) (point-at-bol)))
    (backward-char)))

(defun ale/meow-head ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'meow-head-expand)
    (call-interactively 'meow-head)))

(defun ale/meow-tail ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'meow-tail-expand)
    (call-interactively 'meow-tail)))

(defun ale/meow-backward-delete-char ()
  (interactive)
  (when (and (not (ale/meow--char-readonly))
             (> (point) (point-at-bol)))
    (backward-delete-char 1)))

(defun ale/meow-backward-kill-word ()
  (interactive)
  (unless (ale/meow--char-readonly)
    (backward-kill-word 1)))

(defun ale/meow-kill-whole-line ()
  (interactive)
  (cond ((derived-mode-p 'eshell-mode)
         (eshell-kill-input))
        ((ale/meow--char-readonly)
         nil)
        (t
         (ale/pulse-pulse-line)
         (run-with-timer 0.24 nil 'meow-kill-whole-line 1))))

(defun ale/meow-comment-or-uncomment-region ()
  (interactive)
  (unless (region-active-p) (meow-line 1))
  (call-interactively 'comment-or-uncomment-region))

(defun ale/meow-insert-at-first-non-whitespace ()
  (interactive)
  (back-to-indentation)
  (meow-insert))

(defun ale/meow-top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

(defun ale/meow-save-line ()
  "Temporarily highlight the current line and copy it."
  (interactive)
  (ale/pulse-pulse-line)
  (let ((beg (if (eobp)
                 (line-beginning-position 0)
               (line-beginning-position)))
        (end (line-beginning-position 2)))
    (save-mark-and-excursion
      (goto-char beg)
      (push-mark end t t)
      (meow--execute-kbd-macro meow--kbd-kill-ring-save))))

(defun ale/meow-open-above-or-pop-selection ()
  (interactive)
  (if (region-active-p)
      (meow-pop-selection)
    (meow-open-above)))

(defun ale/meow-open-above-no-insert ()
  (interactive)
  (meow-open-above) (meow-insert-exit))

(defun ale/meow-open-below-no-insert ()
  (interactive)
  (meow-open-below) (meow-insert-exit))

(defun ale/meow--bounds-of-tag ()
  (meow--bounds-of-regexp "<.*>"))

(defun ale/meow--inner-of-tag ()
  (-when-let ((beg . end) (ale/meow--bounds-of-tag))
    (save-mark-and-excursion
      (let (ibeg iend)
        (goto-char beg)
        (setq ibeg (search-forward ">"))
        (goto-char end)
        (setq iend (search-backward "<"))
        (cons ibeg iend)))))

(defun ale/meow-setup ()
  (meow--thing-register 'tag #'ale/meow--inner-of-tag #'ale/meow--bounds-of-tag)
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak)
  (define-key meow-insert-state-keymap (kbd "C-w") #'ale/meow-backward-kill-word)
  (define-key meow-insert-state-keymap (kbd "C-u") #'ale/meow-kill-whole-line)
  (define-key meow-insert-state-keymap (kbd "C-o") #'ale/meow-backward-char)
  (define-key meow-insert-state-keymap (kbd "<C-i>") #'ale/meow-forward-char)
  (define-key meow-insert-state-keymap (kbd "C-;") #'meow-reverse)
  (define-key meow-motion-state-keymap (kbd "<escape>") #'keyboard-escape-quit)
  (define-key minibuffer-local-map (kbd "DEL") #'ale/meow-backward-delete-char)
  (define-key minibuffer-local-map (kbd "C-w") #'backward-kill-word)
  (define-key minibuffer-local-map (kbd "C-u") #'ale/meow-kill-whole-line)
  (define-key minibuffer-local-map (kbd "C-o") #'ale/meow-backward-char)
  (define-key minibuffer-local-map (kbd "<C-i>") #'ale/meow-forward-char)
  (define-prefix-command 'meow-file-command)
  (meow-leader-define-key
   '("SPC" . project-find-file)
   '("ESC" . ale/simple-monocle)
   '("RET" . ibuffer)
   '("?" . describe-keymap)
   '(";" . ale/meow-comment-or-uncomment-region)
   '(":" . eval-expression)
   '("`" . ale/init-edit-config)
   '("/" . lf)
   '("n" . meow-last-buffer)
   '("p" . ale/window-buffers-major-mode)
   '("a" . ace-select-window)
   '("e" . eval-last-sexp)
   '("q" . ale/simple-kill-window-current)
   '("w" . save-buffer)
   '("z" . window-toggle-side-windows)
   '("t" . consult-outline)
   '("s" . meow-keypad-start)
   '("L" . display-line-numbers-mode)
   '("f" . meow-file-command)
   '("f." . (lambda () (interactive) (ale/files-in-user-dirs t)))
   '("fu" . ale/files-in-user-dirs)
   '("fr" . ale/files-recent))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("`" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . beginning-of-buffer)
   '(">" . end-of-buffer)
   '("/" . consult-line)
   '("[" . meow-page-up)
   '("]" . meow-page-down)
   '("-" . ale/meow-open-above-or-pop-selection)
   '("_" . meow-pop-all-selection)
   '("=" . meow-open-below)
   '("'" . meow-end-of-thing)
   '("\\" . meow-pop-search)
   '("<backspace>" . meow-beginning-of-thing)
   '("<C-i>" . better-jumper-jump-forward)
   '("C-o" . better-jumper-jump-backward)
   '("a" . meow-insert)
   '("A" . ale/meow-insert-at-first-non-whitespace)
   '("b" . meow-block)
   '("B" . meow-block-expand)
   '("c" . meow-change)
   '("C" . meow-change-save)
   '("d" . meow-delete)
   '("e" . meow-line)
   '("E" . er/expand-region)
   '("f" . meow-next-word)
   '("F" . meow-next-symbol)
   '("g" . meow-cancel)
   '("h" . (lambda () (interactive) (split-window-right) (windmove-right)))
   '("i" . ale/meow-tail)
   '("I" . ale/meow-open-above-no-insert)
   '("j" . ale/meow-top-join-line)
   '("J" . meow-join)
   '("k" . meow-kill)
   '("K" . meow-kmacro-matches)
   '("l" . meow-kmacro-lines)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . ale/meow-head)
   '("O" . ale/meow-open-below-no-insert)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . meow-quit)
   '("r" . anzu-query-replace)
   '("R" . anzu-query-replace-regexp)
   '("s" . meow-search)
   '("S" . embrace-commander)
   '("t" . meow-append)
   '("T" . meow-append-at-end)
   '("u" . meow-undo)
   '("U" . undo-redo)
   '("v" . (lambda () (interactive) (split-window-below) (windmove-down)))
   '("V" . meow-visit)
   '("w" . meow-back-word)
   '("W" . meow-back-symbol)
   '("x" . meow-save)
   '("X" . meow-clipboard-save)
   '("y" . meow-replace)
   '("Y" . meow-yank-pop)
   '("z" . meow-grab)
   '("Z" . meow-swap-grab)))

(provide 'ale-meow)