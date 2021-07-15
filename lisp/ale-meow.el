(declare-function ale/pulse-pulse-line "ale-pulse")
(require 'ale-window)
(require 'ale-simple)
(require 'dash)

(defun ale/meow-append ()
  (interactive)
  (meow--switch-state 'insert))

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

(defun ale/meow-comment-or-uncomment-region ()
  (interactive)
  (unless (region-active-p) (meow-line 1))
  (call-interactively 'comment-or-uncomment-region))

(defun ale/meow-insert-at-first-non-whitespace ()
  (interactive)
  (back-to-indentation)
  (meow-insert))

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

(defun ale/meow-escape ()
  (interactive)
  (if (region-active-p)
      (meow-cancel)
    (call-interactively 'execute-extended-command)))

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
  (define-key meow-insert-state-keymap (kbd "/") (lambda () (interactive) (self-insert-command 1)))
  (define-key meow-insert-state-keymap (kbd "C-w") #'ale/simple-backward-kill-word)
  (define-key meow-insert-state-keymap (kbd "C-u") #'ale/simple-kill-whole-line)
  (define-key meow-insert-state-keymap (kbd "C-o") #'ale/simple-backward-char)
  (define-key meow-insert-state-keymap (kbd "<C-i>") #'ale/simple-forward-char)
  (define-key meow-insert-state-keymap (kbd "C-;") #'meow-reverse)
  (define-key minibuffer-local-map (kbd "/") (lambda () (interactive) (self-insert-command 1)))
  (define-key minibuffer-local-map (kbd "DEL") #'ale/simple-backward-delete-char)
  (define-key minibuffer-local-map (kbd "C-w") #'backward-kill-word)
  (define-key minibuffer-local-map (kbd "C-u") #'ale/simple-kill-whole-line)
  (define-key minibuffer-local-map (kbd "C-o") #'ale/simple-backward-char)
  (define-key minibuffer-local-map (kbd "<C-i>") #'ale/simple-forward-char)
  (meow-leader-define-key
   '("SPC" . ale/project-find-file)
   '("RET" . ibuffer)
   '("?" . describe-keymap)
   '(";" . ale/meow-comment-or-uncomment-region)
   '("d" . lf-browse-all-directories)
   '("e" . ale/elisp-eval-map)
   '("f" . ale/files-map)
   '("k" . kill-this-buffer)
   '("n" . lf)
   '("o" . ale/utils-map)
   '("p" . ale/window-buffers-major-mode)
   '("s" . ale/simple-map)
   '("t" . ale/consult-map)
   '("w" . save-buffer)
   '("z" . window-toggle-side-windows))
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
   '("-" . good-scroll-down-full-screen)
   '("=" . good-scroll-up-full-screen)
   '("%" . meow-pop-selection)
   '("M-%" . meow-pop-all-selection)
   '("]" . meow-find)
   '("[" . meow-find-expand)
   '("'" . meow-end-of-thing)
   '("\"" . meow-pop-search)
   '("^" . meow-last-buffer)
   '("<backspace>" . meow-beginning-of-thing)
   '("<escape>" . ale/meow-escape)
   '("<C-i>" . better-jumper-jump-forward)
   '("C-o" . better-jumper-jump-backward)
   '("a" . ale/meow-append)
   '("A" . meow-append-at-end)
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
   '("h" . ale/window-split-right)
   '("i" . ale/meow-tail)
   '("I" . ale/meow-insert-at-first-non-whitespace)
   '("j" . ale/simple-top-join-line)
   '("J" . meow-join)
   '("k" . meow-kill)
   '("K" . meow-kmacro-matches)
   '("l" . meow-kmacro-lines)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . ale/meow-head)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . meow-quit)
   '("r" . anzu-query-replace-regexp)
   '("R" . deadgrep)
   '("s" . meow-search)
   '("S" . embrace-commander)
   '("t" . meow-open-below)
   '("T" . meow-open-above)
   '("u" . meow-undo)
   '("U" . undo-redo)
   '("v" . ale/window-split-below)
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
