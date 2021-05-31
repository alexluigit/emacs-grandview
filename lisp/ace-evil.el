(require 'evil)

(defgroup ace/evil ()
  "Extensions for `evil'."
  :group 'evil)

(defun ace/evil-shift-right ()
  (interactive)
  (evil-shift-right evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))

(defun ace/evil-shift-left ()
  (interactive)
  (evil-shift-left evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))

(defun ace/evil-visual-begin-search (beg end direction)
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (let ((selection (regexp-quote (buffer-substring-no-properties beg end))))
      (isearch-update-ring selection evil-regexp-search)
      (isearch-mode direction evil-regexp-search nil t))))

(evil-define-motion ace/evil-visual-search-forward (beg end)
  "Search for the visual selection forwards."
  :jump t
  :repeat nil
  (interactive "<r>")
  (ace/evil-visual-begin-search beg end t))

(evil-define-motion ace/evil-visual-search-backward (beg end)
  "Search for the visual selection backwards."
  :jump t
  :repeat nil
  (interactive "<r>")
  (ace/evil-visual-begin-search beg end nil))

(provide 'ace-evil)
