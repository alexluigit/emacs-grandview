(require 'cl-lib)

(defvar ace/font-size 140)
(defvar ace/zh-font-size 31)
(defvar ace/default-fonts '("Sarasa Mono SC" "Iosevka SS04" "Victor Mono" "Fira Code Retina"))
(defvar ace/fixed-fonts '("Victor Mono" "Fira Code Retina"))
(defvar ace/variable-fonts '("Iosevka SS04" "Victor Mono" "Fira Code Retina"))
(defvar ace/zh-fonts '("Sarasa Mono SC" "Source Han Sans CN" "PingFang SC" "Microsoft Yahei"))
(defvar ace/org-fonts '("Iosevka SS04" "ETBembo" "Fira Code Retina"))
(defvar ace/zh-font-scale 1)

(defun ace/font-chooser (fonts)
  (cl-find-if
   (lambda (f) (if (null (x-list-fonts f)) nil t)) fonts))

(defun ace/font-set ()
  (let ((default (ace/font-chooser ace/default-fonts))
        (fixed (ace/font-chooser ace/fixed-fonts))
        (variable (ace/font-chooser ace/variable-fonts))
        (zh-font (font-spec :family (ace/font-chooser ace/zh-fonts))))
    (setq doom-modeline-icon t)
    (unless (file-exists-p "~/.local/share/fonts/all-the-icons.ttf")
      (all-the-icons-install-fonts t))
    (set-face-attribute 'default nil :font default :height ace/font-size)
    (set-face-attribute 'fixed-pitch nil :font fixed :height ace/font-size)
    (set-face-attribute 'variable-pitch nil :font variable :height ace/font-size)
    (custom-set-faces '(font-lock-keyword-face ((t (:slant italic)))))
    ;; Do not use 'unicode charset, it will cause the English font setting invalid
    (setq face-font-rescale-alist
          (cl-loop for x in ace/zh-fonts
                   collect (cons x ace/zh-font-scale)))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset zh-font))))

(defun ace/font-org-setup ()
  (variable-pitch-mode)
  (org-indent-mode)
  (visual-line-mode)
  (display-line-numbers-mode -1)
  ;; Centering text
  (when (boundp visual-fill-column-mode)
    (setq visual-fill-column-width 120)
    (setq visual-fill-column-center-text t)
    (visual-fill-column-mode 1))
  (setq truncate-lines t)
  ;; Setup font
  (let* ((var-font `(:font ,(ace/font-chooser ace/org-fonts)))
         (base-font-color     (face-foreground 'font-lock-string-face nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
    (custom-theme-set-faces
     'user
     `(org-level-4 ((t (,@headline ,@var-font :height 1.1))))
     `(org-level-3 ((t (,@headline ,@var-font :height 1.2))))
     `(org-level-2 ((t (,@headline ,@var-font :height 1.3))))
     `(org-level-1 ((t (,@headline ,@var-font :height 1.4))))
      ;; ensure that anything that should be fixed-pitch in Org files appears that way
     '(org-block ((t (:inherit fixed-pitch))))
     '(org-code ((t (:inherit (shadow fixed-pitch)))))
     '(org-document-info ((t (:foreground "dark orange"))))
     '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
     '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
     '(org-link ((t (:foreground "royal blue" :underline t))))
     '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     '(org-property-value ((t (:inherit fixed-pitch))) t)
     '(org-checkbox ((t (:inherit fixed-pitch))) t)
     '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
     '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
     '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (ace/font-set))))
  (ace/font-set))

(provide 'ace-fonts)
