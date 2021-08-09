;;; core/autoload/gc.el --- -*- lexical-binding: t -*-

(defvar ale-gc-cons-threshold 134217728 ; 128mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this.  If you experience
stuttering, increase this.")

(defun ale-gc-set-threshold ()
  (setq gc-cons-threshold ale-gc-cons-threshold))

(defun ale-gc-minibuffer-setup-hook ()
  (setq gc-cons-threshold (* ale-gc-cons-threshold 2)))

(defun ale-gc-minibuffer-exit-hook ()
  (garbage-collect) (ale-gc-set-threshold))

;;;###autoload
(defun ale-gc-startup ()
  (ale-gc-set-threshold)
  (if (boundp 'after-focus-change-function)
      (add-function :after after-focus-change-function
                    (lambda () (unless (frame-focus-state) (garbage-collect))))
    (add-hook 'after-focus-change-function 'garbage-collect))
  (add-hook 'minibuffer-setup-hook #'ale-gc-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'ale-gc-minibuffer-exit-hook))

;;;###autoload
(add-hook 'emacs-startup-hook #'ale-gc-startup)

