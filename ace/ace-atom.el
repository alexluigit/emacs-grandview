(defgroup ace-atom ()
  "Auxiliary functions for my dotemacs."
  :group 'editing)

;;;###autoload
(defun ace/atom-number-even-p (n)
  "Test if N is an even number."
  (if (numberp n)
      (= (% n 2) 0)
    (error "%s is not a number" n)))

;;;###autoload
(defun ace/atom-number-integer-p (n)
  "Test if N is an integer."
  (if (integerp n)
      n
    (error "%s is not an integer" n)))

;;;###autoload
(defun ace/atom-number-negative (n)
  "Make N negative."
  (if (numberp n)
      (string-to-number (format "-%d" n)) ; TODO: better way?
    (error "%s is not a number" n)))

;;;###autoload
(defun ace/atom-minor-modes-active ()
  "Return list of active minor modes for the current buffer."
  (let ((active-modes))
    (mapc (lambda (m)
            (when (and (boundp m) (symbol-value m))
              (push m active-modes)))
          minor-mode-list)
    active-modes))

;; Thanks to Omar Antolín Camarena for providing this snippet!
;;;###autoload
(defun ace/atom-completion-table (category candidates)
  "Pass appropriate metadata CATEGORY to completion CANDIDATES.

This is intended for bespoke functions that need to pass
completion metadata that can then be parsed by other
tools (e.g. `embark')."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (category . ,category))
      (complete-with-action action candidates string pred))))

;; Thanks to Igor Lima for the `ace/atom-crm-exclude-selected-p':
;; <https://github.com/0x462e41>.
;; This is used as a filter predicate in the relevant prompts.
(defvar crm-separator)

;;;###autoload
(defun ace/atom-crm-exclude-selected-p (input)
  "Filter out INPUT from `completing-read-multiple'.
Hide non-destructively the selected entries from the completion
table, thus avoiding the risk of inputting the same match twice.

To be used as the PREDICATE of `completing-read-multiple'."
  (if-let* ((pos (string-match-p crm-separator input))
            (rev-input (reverse input))
            (element (reverse
                      (substring rev-input 0
                                 (string-match-p crm-separator rev-input))))
            (flag t))
      (progn
        (while pos
          (if (string= (substring input 0 pos) element)
              (setq pos nil)
            (setq input (substring input (1+ pos))
                  pos (string-match-p crm-separator input)
                  flag (when pos t))))
        (not flag))
    t))

(declare-function auth-source-search "auth-source")

;;;###autoload
(defun ace/atom-auth-get-field (host prop)
  "Find PROP in `auth-sources' for HOST entry."
  (let* ((source (auth-source-search :host host))
         (field (plist-get
                 (flatten-list source)
                 prop)))
    (if source
        field
      (user-error "No entry in auth sources"))))

;; Based on `org--line-empty-p'.
(defmacro ace/atom--line-p (name regexp)
  "Make NAME function to match REGEXP on line n from point."
  `(defun ,name (n)
     (save-excursion
       (goto-char (point-at-bol))
       (and (not (bobp))
	        (or (beginning-of-line n) t)
	        (save-match-data
	          (looking-at ,regexp))))))

(ace/atom--line-p
 ace/atom-empty-line-p
 "[\s\t]*$")

(ace/atom--line-p
 ace/atom-indent-line-p
 "^[\s\t]+")

(ace/atom--line-p
 ace/atom-non-empty-line-p
 "^.+$")

(ace/atom--line-p
 ace/atom-text-list-line-p
 "^\\([\s\t#*+]+\\|[0-9]+[).]+\\)")

(ace/atom--line-p
 ace/atom-text-heading-line-p
 "^[=-]+")

(provide 'ace-atom)
