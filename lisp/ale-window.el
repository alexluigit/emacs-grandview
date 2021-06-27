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
    (ale/windmove--wm-takeover "1"))
   (t
    (let* ((w-list (aw-window-list))
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
    (ale/windmove--wm-takeover "-1"))
   (t
    (let* ((w-list (aw-window-list))
           (old-w-index (cl-position (selected-window) w-list))
           (w-amount (length w-list)))
      (if (= old-w-index 0)
          (ale/window--wm-takeover "-1")
        (select-window (nth (1- old-w-index) w-list))
        (ale/pulse-pulse-line))))))

(provide 'ale-window)
