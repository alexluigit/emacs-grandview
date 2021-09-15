;;; autoload/frame.el --- -*- lexical-binding: t -*-

;;;###autoload
(defun ale-frame-enable (setup-func)
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                `(lambda (f) (with-selected-frame f (,setup-func))))
    (when window-system
      (add-hook 'window-setup-hook `,setup-func))))
