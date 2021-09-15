;;; autoload/orderless.el --- -*- lexical-binding: t -*-

(defun ale-orderless-pinyin-only-initialism (pattern)
  "Leading pinyin initialism regex generator."
  (ale-pinyin-build-regexp-string pattern t nil t))

;;;###autoload
(defun ale-orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher using the equals sign as a prefix."
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

;;;###autoload
(defun ale-orderless-initialism-dispatcher (pattern _index _total)
  "Leading initialism dispatcher using the comma sign as a prefix."
  (when (string-prefix-p "," pattern)
    `(orderless-strict-leading-initialism . ,(substring pattern 1))))

;;;###autoload
(defun ale-orderless-pinyin-dispatcher (pattern _index _total)
  "Pinyin initialism dispatcher using the backtick sign as a prefix."
  (when (string-prefix-p "`" pattern)
    `(ale-orderless-pinyin-only-initialism . ,(substring pattern 1))))

;;;###autoload
(defun ale-orderless-without-literal-dispatcher (pattern _index _total)
  (when (string-prefix-p "~" pattern)
    `(orderless-without-literal . ,(substring pattern 1))))

