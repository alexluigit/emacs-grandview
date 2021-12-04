;;; autoload/fonts.el --- -*- lexical-binding: t -*-

(require 'cl-lib)

(defvar ale-font-size 32)
(defvar ale-default-fonts '("Victor Mono" "Sarasa Mono SC" "Fira Code Retina"))
(defvar ale-fixed-fonts '("Sarasa Mono SC" "Fira Code Retina"))
(defvar ale-variable-fonts '("Sarasa Mono SC" "Victor Mono" "Fira Code Retina"))
(defvar ale-zh-fonts '("FZSuXinShiLiuKaiS-R-GB" "Smartisan Compact CNS" "Sarasa Mono SC" "青鸟华光简报宋二"))
(defvar ale-zh-font-scale 1.2)

;;;###autoload
(defun ale-font-chooser (fonts)
  "Return first valid (exists in OS) font from FONTS."
  (when window-system
    (cl-find-if
     (lambda (f) (if (null (list-fonts (font-spec :family f))) nil t)) fonts)))

;;;###autoload
(defun ale-font-setup ()
  "Setup default/fixed-pitch/variable-pitch/zh-font."
  (interactive)
  (custom-theme-set-faces
   'user
   '(font-lock-keyword-face ((t (:slant italic))))
   '(font-lock-variable-name-face ((t (:weight demibold))))
   '(font-lock-function-name-face ((t (:weight demibold)))))
  (when-let ((default (ale-font-chooser ale-default-fonts))
             (fixed-pitch (ale-font-chooser ale-fixed-fonts))
             (variable-pitch (ale-font-chooser ale-variable-fonts))
             (zh-font (font-spec :family (ale-font-chooser ale-zh-fonts))))
    (custom-theme-set-faces
     'user
     `(default ((t (:font ,(font-spec :family default :size ale-font-size)))))
     `(fixed-pitch ((t (:font ,(font-spec :family fixed-pitch :size ale-font-size)))))
     `(variable-pitch ((t (:font ,(font-spec :family variable-pitch :size ale-font-size))))))
    (unless (equal zh-font (font-spec :family variable-pitch))
      (setq face-font-rescale-alist (list (cons zh-font ale-zh-font-scale))))
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset zh-font))
    (when (< emacs-major-version 28)
      (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji")))))

;;;###autoload
(defun ale-font-cn-set-title (beg end)
  (interactive "r")
  (remove-overlays beg end)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'display '(height 1.5))))

;;;###autoload
(defun ale-font-cn-set-quote (beg end)
  (interactive "r")
  (remove-overlays beg end)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face 'font-lock-comment-face)))
