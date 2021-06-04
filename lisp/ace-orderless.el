;;; -*- lexical-binding: t -*-
(require 'ace-pinyin)

(defun ace/orderless-pinyin-only-initialism (pattern)
  "Leading pinyin initialism regex generator."
  (ace/pinyin-build-regexp-string pattern t nil t))

(defun ace/orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher using the equals sign as a prefix."
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

(defun ace/orderless-initialism-dispatcher (pattern _index _total)
  "Leading initialism dispatcher using the comma sign as a prefix."
  (when (string-prefix-p "," pattern)
    `(orderless-strict-leading-initialism . ,(substring pattern 1))))

(defun ace/orderless-pinyin-dispatcher (pattern _index _total)
  "Pinyin initialism dispatcher using the backtick sign as a prefix."
  (when (string-prefix-p "`" pattern)
    `(ace/orderless-pinyin-only-initialism . ,(substring pattern 1))))

(provide 'ace-orderless)
