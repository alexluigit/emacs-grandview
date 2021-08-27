;;; autoload/modeline.el --- A minimal mode-line. -*- lexical-binding: t; -*-

(use-package mlscroll
  :hook (server-after-make-frame . mlscroll-mode)
  :config
  (setq mlscroll-in-color (face-attribute 'tool-bar :background)))

(defvar flycheck-current-errors)
(defvar flymake-mode-line-format)
(defvar anzu--state)
(defvar anzu--cached-count)
(defvar anzu--overflow-p)
(defvar anzu--current-position)
(defvar anzu--total-matched)
(defvar multiple-cursors-mode)
(declare-function flycheck-count-errors "flycheck" (errors))
(declare-function mc/num-cursors "multiple-cursors" ())

(defgroup ale-modeline nil
  "A minimal mode-line configuration inspired by doom-modeline."
  :group 'mode-line)

(defcustom ale-modeline-show-eol-style nil
  "If t, the EOL style of the current buffer will be displayed in the mode-line."
  :group 'ale-modeline
  :type 'boolean)

(defcustom ale-modeline-show-encoding-information nil
  "If t, the encoding format of the current buffer will be displayed in the mode-line."
  :group 'ale-modeline
  :type 'boolean)

(defcustom ale-modeline-show-cursor-point nil
  "If t, the value of `point' will be displayed next to the cursor position in the mode-line."
  :group 'ale-modeline
  :type 'boolean)

(defface ale-modeline-buffer-name
  '((t (:inherit (mode-line-buffer-id))))
  "Face used for major mode indicator in the mode-line."
  :group 'ale-modeline)

(defface ale-modeline-major-mode
  '((t (:inherit (bold))))
  "Face used for major mode indicator in the mode-line."
  :group 'ale-modeline)

(defface ale-modeline-status-neutral
  '((t (:inherit (shadow))))
  "Face used for neutral or inactive status indicators in the mode-line."
  :group 'ale-modeline)

(defface ale-modeline-status-info
  '((t (:inherit (font-lock-keyword-face))))
  "Face used for generic status indicators in the mode-line."
  :group 'ale-modeline)

(defface ale-modeline-status-success
  '((t (:inherit (success))))
  "Face used for success status indicators in the mode-line."
  :group 'ale-modeline)

(defface ale-modeline-status-warning
  '((t (:inherit (warning))))
  "Face for warning status indicators in the mode-line."
  :group 'ale-modeline)

(defface ale-modeline-status-error
  '((t (:inherit (error))))
  "Face for error stauts indicators in the mode-line."
  :group 'ale-modeline)

(defface ale-modeline-unimportant
  '((t (:inherit (shadow))))
  "Face used for less important mode-line elements."
  :group 'ale-modeline)

(defface ale-modeline-modified
  '((t (:inherit (error))))
  "Face used for the 'modified' indicator symbol in the mode-line."
  :group 'ale-modeline)

(defun ale-modeline--string-trim-left (string)
  "Remove whitespace at the beginning of STRING."
  (if (string-match "\\`[ \t\n\r]+" string)
      (replace-match "" t t string)
    string))

(defun ale-modeline--string-trim-right (string)
  "Remove whitespace at the end of STRING."
  (if (string-match "[ \t\n\r]+\\'" string)
      (replace-match "" t t string)
    string))

(defun ale-modeline--string-trim (string)
  "Remove whitespace at the beginning and end of STRING."
  (ale-modeline--string-trim-left (ale-modeline--string-trim-right string)))

(defun ale-modeline--format (left right)
  "Return a string of `window-width' length containing LEFT and RIGHT, aligned respectively."
  (let ((reserve (string-width right)))
    (concat left
            " "
            (propertize " " 'display
                        `((space :align-to (- (+ right right-fringe right-margin) (+ 1 ,reserve)))))
            right)))

(defvar-local ale-modeline--vc-text nil)
(defun ale-modeline--update-vc-segment (&rest _)
  "Update `ale-modeline--vc-text' against the current VCS state."
  (setq ale-modeline--vc-text
        (when (and vc-mode buffer-file-name)
          (let ((backend (vc-backend buffer-file-name))
                (state (vc-state buffer-file-name (vc-backend buffer-file-name))))
            (let ((face 'mode-line-neutral))
              (concat (cond ((memq state '(edited added))
                             (setq face 'ale-modeline-status-info)
                             (propertize "+ " 'face face))
                            ((eq state 'needs-merge)
                             (setq face 'ale-modeline-status-warning)
                             (propertize "⟷ " 'face face))
                            ((eq state 'needs-update)
                             (setq face 'ale-modeline-status-warning)
                             (propertize "↑ " 'face face))
                            ((memq state '(removed conflict unregistered))
                             (setq face 'ale-modeline-status-error)
                             (propertize "✖ " 'face face))
                            (t
                             (setq face 'ale-modeline-status-neutral)
                             (propertize "✔ " 'face face)))
                      (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                                  'face face
                                  'mouse-face face)
                      "  "))))))

(defvar-local ale-modeline--flycheck-text nil)
(defun ale-modeline--update-flycheck-segment (&optional status)
  "Update `ale-modeline--flycheck-text' against the reported flycheck STATUS."
  (setq ale-modeline--flycheck-text
        (pcase status
          ('finished (if flycheck-current-errors
                         (let-alist (flycheck-count-errors flycheck-current-errors)
                           (let ((sum (+ (or .error 0) (or .warning 0))))
                             (propertize (concat "⚑ Issues: "
                                                 (number-to-string sum)
                                                 "  ")
                                         'face (if .error
                                                   'ale-modeline-status-error
                                                 'ale-modeline-status-warning))))
                       (propertize "✔ Good  " 'face 'ale-modeline-status-success)))
          ('running (propertize "Δ Checking  " 'face 'ale-modeline-status-info))
          ('errored (propertize "✖ Error  " 'face 'ale-modeline-status-error))
          ('interrupted (propertize "⏸ Paused  " 'face 'ale-modeline-status-neutral))
          ('no-checker ""))))

(defvar-local ale-modeline-total-lines nil)
(defun ale-modeline-count-lines ()
  (setq ale-modeline-total-lines (int-to-string (count-lines (point-min) (point-max)))))

(defun ale-modeline-segment-editing-state ()
  "Display current input state."
  (cond ((bound-and-true-p meow-mode)
         (meow-indicator))
        (t
         "")))

(defsubst ale-modeline-segment-macro-recording ()
  "Display current macro being recorded."
  (when (or defining-kbd-macro executing-kbd-macro)
    (propertize " ▶ " 'face 'mode-line-highlight)))

(defun ale-modeline-segment-modified ()
  "Displays a color-coded buffer modification/read-only indicator in the mode-line."
  (if (not (string-match-p "\\*.*\\*" (buffer-name)))
      (if (buffer-modified-p)
          (propertize "● " 'face 'ale-modeline-modified)
        (if (and buffer-read-only (buffer-file-name))
            (propertize "■ " 'face 'ale-modeline-unimportant)
          "  "))
    "  "))

(defun ale-modeline-segment-buffer-name ()
  "Displays the name of the current buffer in the mode-line."
  (propertize "%b  " 'face 'ale-modeline-buffer-name))

(defun ale-modeline-segment-anzu ()
  "Displays color-coded anzu status information in the mode-line (if available)."
  (when (and (boundp 'anzu--state) anzu--state)
    (cond ((eq anzu--state 'replace-query)
           (format #("Replace: %d  " 0 11 (face ale-modeline-status-warning)) anzu--cached-count))
          (anzu--overflow-p
           (format #("%d/%d+  " 0 3 (face ale-modeline-status-info) 3 6 (face ale-modeline-status-error)) anzu--current-position anzu--total-matched))
          (t
           (format #("%d/%d  " 0 5 (face ale-modeline-status-info)) anzu--current-position anzu--total-matched)))))

(defun ale-modeline-segment-multiple-cursors ()
  "Displays the number of active multiple-cursors in the mode-line (if available)."
  (when (and (boundp 'multiple-cursors-mode) multiple-cursors-mode)
    (concat "MC"
            (format #("×%d  " 0 3 (face ale-modeline-status-warning)) (mc/num-cursors)))))

(defun ale-modeline-segment-position ()
  "Displays the current cursor position in the mode-line."
  (concat "%l:%c"
          (when ale-modeline-show-cursor-point (propertize (format ":%d" (point)) 'face 'ale-modeline-unimportant))
          (propertize (concat " " ale-modeline-total-lines) 'face 'ale-modeline-unimportant)))

(defun ale-modeline-segment-eol ()
  "Displays the EOL style of the current buffer in the mode-line."
  (when ale-modeline-show-eol-style
    (pcase (coding-system-eol-type buffer-file-coding-system)
      (0 "LF  ")
      (1 "CRLF  ")
      (2 "CR  "))))

(defun ale-modeline-segment-encoding ()
  "Displays the encoding and EOL style of the buffer in the mode-line."
  (when ale-modeline-show-encoding-information
    (concat (let ((sys (coding-system-plist buffer-file-coding-system)))
              (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                     "UTF-8")
                    (t (upcase (symbol-name (plist-get sys :name))))))
            "  ")))

(defun ale-modeline-segment-vc ()
  "Displays color-coded version control information in the mode-line."
  ale-modeline--vc-text)

(defun ale-modeline-segment-major-mode ()
  "Displays the current major mode in the mode-line."
  (concat (format-mode-line mode-name 'ale-modeline-major-mode) "  "))

(defun ale-modeline-segment-misc-info ()
  "Displays the current value of `mode-line-misc-info' in the mode-line."
  (let ((misc-info (format-mode-line mode-line-misc-info 'ale-modeline-unimportant)))
    (unless (string= (ale-modeline--string-trim misc-info) "")
      (concat (ale-modeline--string-trim misc-info) "  "))))

(defun ale-modeline-segment-flycheck ()
  "Displays color-coded flycheck information in the mode-line (if available)."
  ale-modeline--flycheck-text)

(defun ale-modeline-segment-flymake ()
  "Displays information about the current status of flymake in the mode-line (if available)."
  (when (bound-and-true-p flymake-mode)
    (concat (ale-modeline--string-trim (format-mode-line flymake-mode-line-format)) " ")))

(defun ale-modeline-segment-process ()
  "Displays the current value of `mode-line-process' in the mode-line."
  (let ((process-info (format-mode-line mode-line-process)))
    (unless (string= (ale-modeline--string-trim process-info) "")
      (concat (ale-modeline--string-trim process-info) "  "))))

(defvar ale-modeline--default-mode-line mode-line-format
  "Store the default mode-line format")

;;;###autoload
(define-minor-mode ale-modeline-mode
  "Toggle ale-modeline on or off."
  :group 'ale-modeline
  :global t
  :lighter nil
  (if ale-modeline-mode
      (progn
        (add-hook 'flycheck-status-changed-functions #'ale-modeline--update-flycheck-segment)
        (add-hook 'flycheck-mode-hook #'ale-modeline--update-flycheck-segment)
        (add-hook 'find-file-hook #'ale-modeline--update-vc-segment)
        (add-hook 'after-save-hook #'ale-modeline--update-vc-segment)
        (add-hook 'find-file-hook 'ale-modeline-count-lines)
        (add-hook 'after-save-hook 'ale-modeline-count-lines)
        (add-hook 'after-revert-hook 'ale-modeline-count-lines)
        (advice-add #'vc-refresh-state :after #'ale-modeline--update-vc-segment)
        (setq-default mode-line-format
                      '((:eval
                         (ale-modeline--format
                          (format-mode-line
                           '(" "
                             (:eval (ale-modeline-segment-editing-state))
                             (:eval (ale-modeline-segment-macro-recording))
                             (:eval (ale-modeline-segment-modified))
                             (:eval (ale-modeline-segment-buffer-name))
                             (:eval (ale-modeline-segment-anzu))
                             (:eval (ale-modeline-segment-multiple-cursors))
                             (:eval (ale-modeline-segment-position))))
                          (format-mode-line
                           '((:eval (ale-modeline-segment-eol))
                             (:eval (ale-modeline-segment-encoding))
                             (:eval (ale-modeline-segment-vc))
                             (:eval (ale-modeline-segment-major-mode))
                             (:eval (ale-modeline-segment-misc-info))
                             (:eval (ale-modeline-segment-flycheck))
                             (:eval (ale-modeline-segment-flymake))
                             (:eval (ale-modeline-segment-process))
                             mode-line-end-spaces
                             "          ")))))))
    (progn
      (remove-hook 'find-file-hook #'ale-modeline-count-lines)
      (remove-hook 'after-save-hook #'ale-modeline-count-lines)
      (remove-hook 'after-revert-hook #'ale-modeline-count-lines)
      (remove-hook 'flycheck-status-changed-functions #'ale-modeline--update-flycheck-segment)
      (remove-hook 'flycheck-mode-hook #'ale-modeline--update-flycheck-segment)
      (remove-hook 'file-find-hook #'ale-modeline--update-vc-segment)
      (remove-hook 'after-save-hook #'ale-modeline--update-vc-segment)
      (advice-remove #'vc-refresh-state #'ale-modeline--update-vc-segment)
      (setq-default mode-line-format ale-modeline--default-mode-line))))
