(defun ale/window-split-right ()
  (interactive)
  (split-window-right) (other-window 1))

(defun ale/window-split-below ()
  (interactive)
  (split-window-below) (other-window 1))

;; Copied from ace-window.el
(defun ale/window< (wnd1 wnd2)
  "Return true if WND1 is less than WND2.
This is determined by their respective window coordinates.
Windows are numbered top down, left to right."
  (let* ((f1 (window-frame wnd1))
         (f2 (window-frame wnd2))
         (e1 (window-edges wnd1))
         (e2 (window-edges wnd2))
         (p1 (frame-position f1))
         (p2 (frame-position f2))
         (nl (or (null (car p1)) (null (car p2)))))
    (cond ((and (not nl) (< (car p1) (car p2)))
           t)
          ((and (not nl) (> (car p1) (car p2)))
           nil)
          ((< (car e1) (car e2))
           t)
          ((> (car e1) (car e2))
           nil)
          ((< (cadr e1) (cadr e2))
           t))))

(defun ale/window-list ()
  (sort (window-list) 'ale/window<))

(defun ale/window-select-by-index (index)
  (select-window (nth index (ale/window-list))))

(provide 'ale-window)
