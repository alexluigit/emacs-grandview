(require 'cl)                         ; for find if

(defvar ace/default-font-size 140)
(defvar ace/default-variable-font-size 140)
(defvar en-font-list '("Iosevka SS04" "Victor Mono" "Microsoft Yahei" "STHeiti"))
(defvar zh-font-list '("Sarasa Mono SC" "PingFang SC" "Microsoft Yahei"))

(defun ace/font--existsp (font)
  (if (null (x-list-fonts font)) nil t))

(defun ace/font-set (english-fonts english-font-size chinese-fonts &optional chinese-font-scale)
  "english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-scale to nil, it will follow english-font-size"
  (setq chinese-font-scale (or chinese-font-scale 1.2))
  (setq face-font-rescale-alist
        (cl-loop for x in zh-font-list
              collect (cons x chinese-font-scale)))
  ;; (let ((en-font (ace/font--make-string
  ;;                 (find-if #'ace/font--existsp english-fonts)
  ;;                 english-font-size))
  (let ((en-font (find-if #'ace/font--existsp english-fonts))
        (zh-font (font-spec :family (find-if #'ace/font--existsp chinese-fonts))))
    ;; Set the default English font
    (message "Set English Font to %s" en-font)
    (set-face-attribute 'default nil :font en-font :height english-font-size)
    ;; Set Chinese font
    ;; Do not use 'unicode charset, it will cause the English font setting invalid
    (message "Set Chinese Font to %s" zh-font)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset zh-font))))

(provide 'ace-fonts)
