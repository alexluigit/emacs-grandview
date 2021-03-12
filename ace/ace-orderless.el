(defgroup ace/orderless ()
  "Tweaks for the Orderless completion style."
  :group 'minibuffer)

(defcustom ace/orderless-default-styles
  '(orderless-flex
    orderless-strict-leading-initialism
    orderless-regexp
    orderless-prefixes
    orderless-literal)
  "List that should be assigned to `orderless-matching-styles'."
  :type 'list
  :group 'ace/orderless)

(defcustom ace/orderless-alternative-styles
  '(orderless-literal
    orderless-prefixes
    orderless-strict-leading-initialism
    orderless-regexp)
  "Alternative list for `orderless-matching-styles'.

Unlike `ace/orderless-default-styles', this variable is intended
for use on a case-by-case basis, with the help of the function
`ace/orderless-with-styles'."
  :type 'list
  :group 'ace/orderless)

(defun ace/orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

(defun ace/orderless-initialism-dispatcher (pattern _index _total)
  "Leading initialism  dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "," pattern)
    `(orderless-strict-leading-initialism . ,(substring pattern 0 -1))))

(defvar orderless-matching-styles)

;;;###autoload
(defun ace/orderless-with-styles (cmd &optional styles)
  "Call CMD with optional orderless STYLES.

STYLES is a list of pattern matching methods that is passed to
`orderless-matching-styles'.  Its fallback value is that of
`ace/orderless-alternative-styles'."
  (let ((orderless-matching-styles (or styles ace/orderless-alternative-styles))
        (this-command cmd))
    (call-interactively cmd)))

(provide 'ace-orderless)
