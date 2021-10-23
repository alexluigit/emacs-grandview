;;; autoload/fonts.el --- -*- lexical-binding: t -*-

(require 'cl-lib)

(defvar ale-font-size 32)
(defvar ale-default-fonts '("Victor Mono" "Iosevka SS04" "Sarasa Mono SC" "Fira Code Retina"))
(defvar ale-fixed-fonts '("Victor Mono" "Fira Code Retina"))
(defvar ale-variable-fonts '("Sarasa Mono SC" "Victor Mono" "Fira Code Retina"))
(defvar ale-zh-fonts '("FZSuXinShiLiuKaiS-R-GB" "Smartisan Compact CNS" "Sarasa Mono SC" "青鸟华光简报宋二"))
(defvar ale-org-fonts '("Sarasa Mono SC" "ETBembo" "Fira Code Retina"))
(defvar ale-emoji-font (font-spec :family "Noto Color Emoji" :width 'normal :slant 'normal))
(defvar ale-zh-font-scale 1.2)

;;;###autoload
(defun ale-font-chooser (fonts)
  (cl-find-if
   (lambda (f) (if (null (x-list-fonts f)) nil t)) fonts))

;;;###autoload
(defun ale-font-setup ()
  (interactive)
  (let ((default (ale-font-chooser ale-default-fonts))
        (fixed (ale-font-chooser ale-fixed-fonts))
        (variable (ale-font-chooser ale-variable-fonts))
        (zh-font (font-spec :family (ale-font-chooser ale-zh-fonts))))
    (set-face-attribute 'default nil :font (font-spec :family default :size ale-font-size))
    (set-face-attribute 'fixed-pitch nil :font (font-spec :family fixed :size ale-font-size))
    (set-face-attribute 'variable-pitch nil :font (font-spec :family variable :size ale-font-size))
    (custom-set-faces '(font-lock-keyword-face ((t (:slant italic)))))
    (custom-set-faces '(font-lock-variable-name-face ((t (:weight demibold)))))
    (custom-set-faces '(font-lock-function-name-face ((t (:weight demibold)))))
    (set-fontset-font t 'symbol ale-emoji-font)
    (unless (member zh-font ale-variable-fonts)
      (setq face-font-rescale-alist (list (cons zh-font ale-zh-font-scale))))
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset zh-font))))

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
