;;; core/autoload/fonts.el --- -*- lexical-binding: t -*-

(require 'cl-lib)

(defvar ale-font-size 32)
(defvar ale-default-fonts '("Victor Mono" "Iosevka SS04" "Sarasa Mono SC" "Fira Code Retina"))
(defvar ale-fixed-fonts '("Victor Mono" "Fira Code Retina"))
(defvar ale-variable-fonts '("Sarasa Mono SC" "Victor Mono" "Fira Code Retina"))
(defvar ale-zh-fonts '("Sarasa Mono SC" "Source Han Sans CN" "PingFang SC" "Microsoft Yahei"))
(defvar ale-org-fonts '("Sarasa Mono SC" "ETBembo" "Fira Code Retina"))
(defvar ale-emoji-font (font-spec :family "Noto Color Emoji" :width 'normal :slant 'normal))
(defvar ale-zh-font-scale 1)

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
    (setq doom-modeline-icon t)
    (unless (file-exists-p "~/.local/share/fonts/all-the-icons.ttf")
      (all-the-icons-install-fonts t))
    (set-face-attribute 'default nil :font (font-spec :family default :size ale-font-size))
    (set-face-attribute 'fixed-pitch nil :font (font-spec :family fixed :size ale-font-size))
    (set-face-attribute 'variable-pitch nil :font (font-spec :family variable :size ale-font-size))
    (custom-set-faces '(font-lock-keyword-face ((t (:slant italic)))))
    (custom-set-faces '(font-lock-variable-name-face ((t (:weight demibold)))))
    (custom-set-faces '(font-lock-function-name-face ((t (:weight demibold)))))
    (ale-font-emoji-setup)
    (setq face-font-rescale-alist
          (cl-loop for x in ale-zh-fonts
                   collect (cons x ale-zh-font-scale)))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset zh-font))))

;;;###autoload
(defun ale-font-emoji-setup ()
  "Setting up emoji display and composition for emoji modifiers."
  (set-fontset-font t 'symbol ale-emoji-font)
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
