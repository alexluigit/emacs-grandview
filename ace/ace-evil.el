(when (featurep 'evil)
  (require 'evil))

(defgroup ace/evil ()
  "Extensions for `evil'."
  :group 'evil)

(defun ace/evil-tab ()
  "Do org-cycle if in org-mode, otherwise go to end of line."
  (interactive)
  ;; (if (stringp mode-name)
  (if (and (stringp mode-name) (string= mode-name "Org"))
      (org-cycle)
    (evil-end-of-line)))

(defcustom ace/evil-visualstar-persistent nil
  "Set to `t` if `*` and `#` should keep visual-mode.
That would visually-select the found occurrence, allowing for
repeated searches.
You will need to hit escape to leave visual-mode."
  :group 'ace/evil
  :type 'boolean)

(defun ace/evil-visualstar-begin-search (beg end direction)
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (let ((found)
          (selection (regexp-quote (buffer-substring-no-properties beg end))))
      (if (eq evil-search-module 'isearch)
          (progn
            (setq isearch-forward direction)
            (setq found (evil-search selection direction t)))
        (let ((pattern (evil-ex-make-search-pattern selection))
              (direction (if direction 'forward 'backward)))
          (setq evil-ex-search-direction direction)
          (setq evil-ex-search-pattern pattern)
          (evil-ex-search-activate-highlight pattern)
          ;; update search history unless this pattern equals the
          ;; previous pattern
          (unless (equal (car-safe evil-ex-search-history) selection)
            (push selection evil-ex-search-history))
          (evil-push-search-history selection (eq direction 'forward))
          (setq found (evil-ex-search-next))))
      (when (and ace/evil-visualstar-persistent found)
        (push-mark (+ (point) (- end beg)) nil t)))))

(evil-define-motion ace/evil-visualstar-begin-search-forward (beg end)
  "Search for the visual selection forwards."
  :jump t
  :repeat nil
  (interactive "<r>")
  (ace/evil-visualstar-begin-search beg end t))

(evil-define-motion ace/evil-visualstar-begin-search-backward (beg end)
  "Search for the visual selection backwards."
  :jump t
  :repeat nil
  (interactive "<r>")
  (ace/evil-visualstar-begin-search beg end nil))

;;;###autoload
(define-minor-mode ace/evil-visualstar-mode
  "Minor mode for visual star selection."
  :keymap (let ((map (make-sparse-keymap)))
            (evil-define-key 'visual map (kbd "*") #'ace/evil-visualstar-begin-search-forward)
            (evil-define-key 'visual map (kbd "#") #'ace/evil-visualstar-begin-search-backward)
            map)
  (evil-normalize-keymaps))

;;;###autoload
(define-globalized-minor-mode ace/global-evil-visualstar-mode
  ace/evil-visualstar-mode ace/turn-on-evil-visualstar-mode)

;;;###autoload
(defun ace/turn-on-evil-visualstar-mode ()
  "Turns on visual star selection."
  (interactive)
  (ace/evil-visualstar-mode t))

;;;###autoload
(defun ace/turn-off-evil-visualstar-mode ()
  "Turns off visual star selection."
  (interactive)
  (ace/evil-visualstar-mode -1))

(provide 'ace-evil)
