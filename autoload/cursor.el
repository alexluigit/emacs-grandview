;;; autoload/cursor.el --- -*- lexical-binding: t -*-

(defvar ale-cursor-saved-color (frame-parameter nil 'cursor-color))

(defcustom ale-cursor-dim-color "#606060"
  "Cursor color for `ale-dim-cursor-mode'."
  :group 'cursor :type 'string)

;;;###autoload
(define-minor-mode ale-cursor-dim-mode
  "Enable dimmed `cursor-color'."
  :global t
  :lighter nil
  :group 'cursor
  (if ale-cursor-dim-mode
      (progn
        (setq-local cursor-type nil)
        (blink-cursor-mode -1)
        (set-cursor-color ale-cursor-dim-color))
    (blink-cursor-mode +1)
    (set-cursor-color ale-cursor-saved-color)))
