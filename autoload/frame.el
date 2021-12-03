;;; autoload/frame.el --- -*- lexical-binding: t -*-

;;;###autoload
(defun ale-frame-enable (setup-func)
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                `(lambda (f) (with-selected-frame f (,setup-func))))
    (if window-system
        (add-hook 'window-setup-hook `,setup-func)
      (add-hook 'after-init-hook `,setup-func))))

(provide 'ale-frame)
