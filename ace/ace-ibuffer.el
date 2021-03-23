(require 'ibuffer)

;;;###autoload
(defun ace/ibuffer-buffers-major-mode (&optional arg)
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

;;;###autoload
(defun ace/ibuffer-buffers-vc-root (&optional arg)
  "Select buffers that belong to the version controlled directory.
With optional prefix ARG (\\[universal-argument]) produce an
`ibuffer' filtered accordingly.  Else use standard completion."
  (interactive "P")
  (let* ((root (or (vc-root-dir)
                   (locate-dominating-file "." ".git")))
         (prompt "Buffers for VC"))
    (if root
        (if arg
            (ibuffer t (format "*%s %s*" prompt root)
                     (list (cons 'filename (expand-file-name root))))
          (switch-to-buffer
           (read-buffer
            (format "%s %s:" prompt root) nil t
            (lambda (pair) ; pair is (name-string . buffer-object)
              (with-current-buffer (cdr pair) (string= (vc-root-dir) root))))))
      (user-error "Not in a version-controlled directory"))))

(provide 'ace-ibuffer)
