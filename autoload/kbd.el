;;; autoload/kbd.el --- -*- lexical-binding: t -*-

;;;###autoload
(defun ale-kbd-C-i-fix ()
  "Make emacs differentiate C-i and Tab keys.

For historical reason, terminal can not tell the difference between
some key storkes. For example, `C-i' and `<tab>', `C-m' and `Return',
etc. By default, emacs follow this convention, but it doesn't mean
emacs are not able to tell the difference. To change this behaviour,
we can use `input-decode-map' to give `C-i' different meaning."
  (if IS-GUI
      (add-hook 'after-make-frame-functions
                (lambda (f) (with-selected-frame f (ale-kbd--C-i-fix-GUI))))
    (add-hook 'window-setup-hook 'ale-kbd--C-i-fix-TERM)))

(defun ale-kbd--C-i-fix-GUI ()
  "Helper for `ale-kbd-C-i-fix'."
  (define-key input-decode-map [?\C-i] [C-i]))

(defun ale-kbd--C-i-fix-TERM ()
  "Helper for `ale-kbd-C-i-fix'."
  (bind-keys
   ("<f6>" . better-jumper-jump-forward)
   :map minibuffer-local-map
   ("<f6>" . forward-char)
   :map meow-insert-state-keymap
   ("<f6>" . ale-insert-ctrl-i)))
