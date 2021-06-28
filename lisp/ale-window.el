(defun ale/window-split-right ()
  (interactive)
  (split-window-right) (other-window 1))

(defun ale/window-split-below ()
  (interactive)
  (split-window-below) (other-window 1))

(defun ale/window-buffers-major-mode (&optional arg)
  "Select buffers to switch to. Same as `switch-to-buffer'.
With optional prefix ARG (\\[universal-argument]) select buffers
that match the current buffer's major mode."
  (interactive "P")
  (let* ((major major-mode)
         (prompt "Buffers for"))
    (if arg
        (switch-to-buffer
              (read-buffer
                (format "%s %s:" prompt major) nil t
                (lambda (pair) ; pair is (name-string . buffer-object)
                  (with-current-buffer (cdr pair) (derived-mode-p major)))))
      (switch-to-buffer (read-buffer "Switch to buffer: ")))))

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

(defun ale/window--wm-takeover (direction)
  (let ((cmd "awesome-client")
        (args (concat
               "require(\"awful\").client.focus.byidx\("
               direction "\)")))
    (start-process "" nil cmd args)))

(defun ale/window-move-next ()
  (interactive)
  (cond
   ((and (featurep 'lf) (lf-live-p))
    (ale/window--wm-takeover "1"))
   (t
    (let* ((w-list (ale/window-list))
           (old-w-index (cl-position (selected-window) w-list))
           (w-amount (length w-list)))
      (if (>= old-w-index (1- w-amount))
          (ale/window--wm-takeover "1")
        (select-window (nth (1+ old-w-index) w-list))
        (ale/pulse-pulse-line))))))

(defun ale/window-move-prev ()
  (interactive)
  (cond
   ((and (featurep 'lf) (lf-live-p))
    (ale/window--wm-takeover "-1"))
   (t
    (let* ((w-list (ale/window-list))
           (old-w-index (cl-position (selected-window) w-list))
           (w-amount (length w-list)))
      (if (= old-w-index 0)
          (ale/window--wm-takeover "-1")
        (select-window (nth (1- old-w-index) w-list))
        (ale/pulse-pulse-line))))))

(provide 'ale-window)
