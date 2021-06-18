(defun ace-modeline-nov ()
  (setq-local mode-line-format
              `((:eval
                 (doom-modeline-segment--workspace-name))
                (:eval
                 (doom-modeline-segment--window-number))
                (:eval
                 (concat
                  " "
                  (propertize
                   (cdr (assoc 'creator nov-metadata))
                   'face 'doom-modeline-project-parent-dir)
                  " "
                  (cdr (assoc 'title nov-metadata))
                  " "
                  (propertize
                   (format "%d/%d"
                           (1+ nov-documents-index)
                           (length nov-documents))
                   'face 'doom-modeline-info)))
                ,(propertize
                  " %P "
                  'face 'doom-modeline-buffer-minor-mode)
                ,(propertize
                  " "
                  'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)
                  'display `((space
                              :align-to
                              (- (+ right right-fringe right-margin)
                                 ,(* (let ((width (doom-modeline--font-width)))
                                       (or (and (= width 1) 1)
                                           (/ width (frame-char-width) 1.0)))
                                     (string-width
                                      (format-mode-line
                                       (cons "" '(:eval (doom-modeline-segment--major-mode))))))))))
                (:eval (doom-modeline-segment--major-mode)))))

(provide 'ace-modeline)
