(defun ale/window-split-right ()
  (interactive)
  (split-window-right) (other-window 1))

(defun ale/window-split-below ()
  (interactive)
  (split-window-below) (other-window 1))

(provide 'ale-window)
