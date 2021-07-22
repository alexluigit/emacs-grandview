;;; -*- lexical-binding: t -*-
(require 'cl-lib)

(defvar ale/font-size 32)
(defvar ale/default-fonts '("Sarasa Mono SC" "Iosevka SS04" "Victor Mono" "Fira Code Retina"))
(defvar ale/fixed-fonts '("Victor Mono" "Fira Code Retina"))
(defvar ale/variable-fonts '("Sarasa Mono SC" "Victor Mono" "Fira Code Retina"))
(defvar ale/zh-fonts '("Sarasa Mono SC" "Source Han Sans CN" "PingFang SC" "Microsoft Yahei"))
(defvar ale/org-fonts '("Sarasa Mono SC" "ETBembo" "Fira Code Retina"))
(defvar ale/emoji-font (font-spec :family "Noto Color Emoji" :width 'normal :slant 'normal))
(defvar ale/zh-font-scale 1)

(defun ale/font-chooser (fonts)
  (cl-find-if
   (lambda (f) (if (null (x-list-fonts f)) nil t)) fonts))

(defun ale/font-setup ()
  (interactive)
  (let ((default (ale/font-chooser ale/default-fonts))
        (fixed (ale/font-chooser ale/fixed-fonts))
        (variable (ale/font-chooser ale/variable-fonts))
        (zh-font (font-spec :family (ale/font-chooser ale/zh-fonts))))
    (setq doom-modeline-icon t)
    (unless (file-exists-p "~/.local/share/fonts/all-the-icons.ttf")
      (all-the-icons-install-fonts t))
    (set-face-attribute 'default nil :font (font-spec :family default :size ale/font-size))
    (set-face-attribute 'fixed-pitch nil :font (font-spec :family fixed :size ale/font-size))
    (set-face-attribute 'variable-pitch nil :font (font-spec :family variable :size ale/font-size))
    (custom-set-faces '(font-lock-keyword-face ((t (:slant italic)))))
    (custom-set-faces '(font-lock-variable-name-face ((t (:weight demibold)))))
    (custom-set-faces '(font-lock-function-name-face ((t (:weight demibold)))))
    (ale/emoji-setup)
    (setq face-font-rescale-alist
          (cl-loop for x in ale/zh-fonts
                   collect (cons x ale/zh-font-scale)))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset zh-font))))

(defun ale/emoji-setup ()
  "Setting up emoji display and composition for emoji modifiers."
  (set-fontset-font t 'symbol ale/emoji-font)
  (cl-dolist (items `(((?🇦 . ?🇿) [".[🇦-🇿]+" 0 font-shape-gstring])
                      ((?🏳 . ?🏴) [".[️‍🌈⚧☠󠀠-󠁿]*" 0 font-shape-gstring])
                      (?⃣ ["[#*0-9]️⃣" 2 font-shape-gstring])
                      ;; TODO: I can't make keycap sequences work because I think they're trying to shape with the wrong font.
                      ,@(mapcar (lambda (range) (list range [".‍?[🏻-🏿]?[‍️♂♀]*️?" 0 font-shape-gstring]))
                                (cl-concatenate 'list "☝🎅🏇👂👃👦👧👼💏💑💪🕴🕵🕺🖐🖕🖖🙇🚣🛀🛌🤏🤞🤟🤦🤽🤾🥷🦻👯❤"
                                                '((?⛹ . ?✍) (?🏂 . ?🏄) (?🏊 . ?🏌) (?👆 . ?👐)
                                                  (?👫 . ?👮) (?👰 . ?👸) (?💁 . ?💇) (?🙅 . ?🙇) (?🙋 . ?🙏)
                                                  (?🚴 . ?🚶) (?🤘 . ?🤜) (?🤰 . ?🤹) (?🤼 . ?🤾) (?🦵 . ?🦹)
                                                  (?🧍 . ?🧏) (?🧒 . ?🧟))))
                      (?🧑 [".‍?[🏻-🏿]?[‍⚕⚖✈❤️🌾🍳🍼🎄🎓🎤🎨🏫🏭👦-👩💋💻💼🔧🔬🚀🚒🤝🦯🦰-🦳🦼🦽🧑]*" 0 font-shape-gstring])
                      ((?👨 . ?👩) [".‍?[🏻-🏿]?[‍⚕⚖✈❤️🌾🍳🍼🎄🎓🎤🎨🏫🏭👦-👩💋💻💼🔧🔬🚀🚒🤝🦯🦰-🦳🦼🦽🧑]*" 0 font-shape-gstring])
                      ,@(mapcar (lambda (str) (list (elt str 0) (vector str 0 'font-shape-gstring)))
                                '("😶‍🌫️" "🐈‍⬛" "🐕‍🦺" "🐻‍❄️" "👁️‍🗨️" "😮‍💨" "😵‍💫"))))
    (set-char-table-range composition-function-table (car items) (list (cadr items)))))

(defun ale/font-org-setup ()
  (interactive)
  (variable-pitch-mode)
  (org-indent-mode)
  (visual-line-mode)
  (display-line-numbers-mode -1)
  ;; Centering text
  (when (fboundp 'visual-fill-column-mode)
    (setq visual-fill-column-width 120)
    (setq visual-fill-column-center-text t)
    (visual-fill-column-mode 1))
  ;; Setup font
  (let ((var-font `(:font ,(ale/font-chooser ale/org-fonts))))
    (custom-theme-set-faces
     'user
     `(org-level-4 ((t (,@var-font :height 1.2))))
     `(org-level-3 ((t (,@var-font :height 1.3))))
     `(org-level-2 ((t (,@var-font :height 1.4))))
     `(org-level-1 ((t (,@var-font :height 1.5))))
     ;; ensure that anything that should be fixed-pitch in Org files appears that way
     '(org-block ((t (:inherit fixed-pitch))))
     '(org-code ((t (:inherit (shadow fixed-pitch)))))
     '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
     '(org-link ((t (:foreground "royal blue" :underline t))))
     '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     '(org-property-value ((t (:inherit fixed-pitch))) t)
     '(org-checkbox ((t (:inherit fixed-pitch))) t)
     '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
     '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (ale/font-setup))))
  (when (display-graphic-p) (ale/font-setup)))

(provide 'ale-fonts)
