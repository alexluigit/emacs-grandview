;;; -*- lexical-binding: t -*-
(require 'ale-pinyin)

(defun ale/orderless-pinyin-only-initialism (pattern)
  "Leading pinyin initialism regex generator."
  (ale/pinyin-build-regexp-string pattern t nil t))

(defun ale/orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher using the equals sign as a prefix."
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

(defun ale/orderless-initialism-dispatcher (pattern _index _total)
  "Leading initialism dispatcher using the comma sign as a prefix."
  (when (string-prefix-p "," pattern)
    `(orderless-strict-leading-initialism . ,(substring pattern 1))))

(defun ale/orderless-pinyin-dispatcher (pattern _index _total)
  "Pinyin initialism dispatcher using the backtick sign as a prefix."
  (when (string-prefix-p "`" pattern)
    `(ale/orderless-pinyin-only-initialism . ,(substring pattern 1))))

(provide 'ale-orderless)
