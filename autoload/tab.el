;;; autoload/tab.el --- Tabs for frequently used buffers -*- lexical-binding: t -*-

(require 'cl-lib)

(defvar ale-tab-count-freq-idle-time 1
  "Add used frequency after being idle for this much secs.")

(defvar ale-tab-visible-buffer-limit 4
  "Max number of visible buffers in a group.")

(defvar ale-tab-project-root-function
  (lambda ()
    (when-let ((project (project-current nil)))
      (expand-file-name (cdr project))))
  "A function that returns project root for current buffer.")

(defvar ale-tab-separator "|"
  "A string used as separator between tabs.")

(defvar ale-tab-update-hook nil
  "Hook to run when tabs need to be redisplayed.
You should customize this hook to run code that's needed to
update the UI.  `ale-tab-string' can be used in the code.")

(defface ale-tab-current-tab-face
  '((((background light))
     :background "#d5c9c0" :foreground "#282828"
     :bold t :inherit mode-line-active)
    (t
     :background "#504945" :foreground "#fbf1c7"
     :bold t :inherit mode-line-active))
  "Face for current tab.")

(defface ale-tab-inactive-tab-face
  '((((background light))
     :foreground "#665c54" :inherit 'mode-line-active)
    (t
     :foreground "#bdae93" :bold nil :inherit 'mode-line-active))
  "Face for inactive tabs.")

(defface ale-tab-separator-face
  '((((background light))
     :foreground "#bdae93" :bold t :inherit 'mode-line-active)
    (t
     :foreground "#665c54" :bold t :inherit 'mode-line-active))
  "Face for separator.")

(defface ale-tab-rest-face
  '((t :italic t :bold nil :inherit 'mode-line-active))
  "Face for current tab.")

(defvar-local ale-tab/buffer-group nil)

(defun ale-tab/buffer-group (&optional buffer)
  "Return the group name of BUFFER.
When BUFFER is nil, use current buffer."
  (let* ((buffer (or buffer (current-buffer)))
         (name (buffer-name buffer))
         group)
    (cond
     ;; These should be hidden.
     ((eq (aref name 0) ?\s) nil)
     ((cl-some (lambda (common-name-prefix)
                 (string-prefix-p common-name-prefix name))
               '("*Backtrace" "*scratch" "*Faces" "*Messages"
                 "*Customize" "*Warnings" "*lsp-"))
      "*Common*")
     ((setq group (or (buffer-local-value 'ale-tab/buffer-group buffer)
                      (with-current-buffer buffer
                        (setq ale-tab/buffer-group
                              (funcall ale-tab-project-root-function)))))
      group)
     ((eq (aref name 0) ?*) "*Common*")
     (t "*Others*"))))

(defvar ale-tab/timer nil
  "Timer used for count buffer used frequency.")

(defvar-local ale-tab/buffer-freq 0
  "Used frequency of current buffer.")

(defun ale-tab/buffer-freq (buf)
  "Return the used frequency of buffer BUF."
  (or (buffer-local-value 'ale-tab/buffer-freq buf)
      0))

(defun ale-tab/increase-buffer-freq ()
  "Increase the used frequency of current buffer by 1."
  (cl-incf ale-tab/buffer-freq))

(defun ale-tab/buffer-freq-higher-p (buf1 buf2)
  "Return t if the used frequency of BUF1 is higher than BUF2."
  (> (ale-tab/buffer-freq buf1)
     (ale-tab/buffer-freq buf2)))

(defun ale-tab/insert-buf (buf bufs)
  "Insert BUF into sorted BUFS.
BUFS is sorted in the decreasing order of used frequency.  The
insertion keeps this order.
This is non-destructive and the list after insertion is returned."
  (let ((freqs (mapcar #'ale-tab/buffer-freq bufs))
        (freq (ale-tab/buffer-freq buf))
        idx)
    (cl-dotimes (n (length freqs))
      (when (> freq (nth n freqs))
        (setq idx n)
        (cl-return nil)))
    (if (null idx)
        (append bufs (list buf))
      (append (cl-subseq bufs 0 idx)
              (list buf)
              (cl-subseq bufs idx)))))

(defun ale-tab-start-count-freq ()
  "Start counting buffer used frequency."
  (setq ale-tab/timer
        (run-with-idle-timer ale-tab-count-freq-idle-time
                             t #'ale-tab/increase-buffer-freq)))

(defun ale-tab-stop-count-freq ()
  "Stop counting buffer used frequency."
  (cancel-timer ale-tab/timer)
  (setq ale-tab/timer nil))

(defvar ale-tab/sorted-buffer-list nil
  "Buffer list sorted by used frequency.
This contains all non-hidden buffers returned by `buffer-list'.
It's updated by `ale-tab/update-buffer-list'.")

(defvar ale-tab/last-active-buffer nil
  "Last active buffer.
Minibuffer doesn't count.  This is updated by
`ale-tab/update-buffer-list'.")

(defvar ale-tab/inhibit-resort nil
  "Non-nil means don't resort `ale-tab/sorted-buffer-list'.")

(defun ale-tab/update-buffer-list ()
  "Update internal data when appropriate."
  (unless (window-minibuffer-p)
    (let ((current-buffer (current-buffer)))
      ;; We don't update when the current buffer is not changed, and non of the
      ;; non-hidden buffers is killed.
      (unless (and (eq current-buffer ale-tab/last-active-buffer)
                   (cl-every #'buffer-live-p
                             ale-tab/sorted-buffer-list))
        (unless ale-tab/inhibit-resort
          (let ((bufs (buffer-list)))
            (setq bufs (cl-remove-if-not #'ale-tab/buffer-group bufs))
            (setq bufs (sort bufs #'ale-tab/buffer-freq-higher-p))
            (setq ale-tab/sorted-buffer-list bufs)))
        (run-hooks 'ale-tab-update-hook)
        (setq ale-tab/last-active-buffer (current-buffer))))))

(defun ale-tab-visible-tabs-and-remain-num ()
  "Return the visible tabs and number of remaining tabs in a cons cell.
When the current buffer is a hidden buffer, return nil."
  (let* ((buf (current-buffer))
         (group (ale-tab/buffer-group buf))
         (counter 0)
         tabs)
      (when group
        (dolist (b ale-tab/sorted-buffer-list)
          (when (equal (ale-tab/buffer-group b) group)
            (if (< (length tabs) ale-tab-visible-buffer-limit)
                (push b tabs)
              (cl-incf counter))))
        (cons (nreverse tabs)
              counter))))

(defun ale-tab-visible-tabs ()
  "Return the visible tabs."
  (let* ((buf (current-buffer))
         (group (ale-tab/buffer-group buf))
         tabs)
      (when group
        (cl-dolist (b ale-tab/sorted-buffer-list)
          (when (equal (ale-tab/buffer-group b) group)
            (push b tabs)
            (when (= (length tabs) ale-tab-visible-buffer-limit)
              (cl-return))))
        (nreverse tabs))))

(defun ale-tab-string ()
  "Return a string that shows the tabs for current buffer.
Possible ways of using this string is to show it in the mode line
or header line, see `mode-line-format' and `header-line-format'
for instructions.  See \"site-lisp/toki-modeline.el\" for
example.
The string may look like
    tab1 | tab2 | 2+..tab5
\"tab1\" and \"tab2\" are the actual tabs, \"2+\" means there are
2 more buffers in the current group that's not shown.  When the
current buffer is one of these invisible buffers (\"tab5\" in
this case), it is shown after the \"1+\" part.
Notice that though \"tab5\" is shown to indicate the current
buffer, technically it's not in the tabs, and is still considered
\"invisible\" by `ale-tab-kill-invisible-buffers-in-group'.
Current and non-active buffers are distinguished by faces."
  (let* ((current-buf (current-buffer))
         (tabs-and-remain (ale-tab-visible-tabs-and-remain-num))
         (tabs (car tabs-and-remain))
         (tab-visible-p (memq current-buf tabs))
         (num (cdr tabs-and-remain))
         (rest (unless (or (eq num 0) (null num))
                 (propertize (concat "+" (number-to-string num) "..")
                             'face 'ale-tab-rest-face)))
         (get-string (lambda (buf)
                       (if (eq buf current-buf)
                           (propertize (concat " " (buffer-name buf) " ")
                                       'face 'ale-tab-current-tab-face)
                         (propertize (concat " " (buffer-name buf) " ")
                                     'face 'ale-tab-inactive-tab-face))))
         (separator (propertize ale-tab-separator
                                'face 'ale-tab-separator-face)))
    (when (and rest (not tab-visible-p))
      (setq rest (concat rest
                         (propertize (buffer-name current-buf)
                                     'face 'ale-tab-current-tab-face))))
    (if tabs
        (string-join (nconc (mapcar get-string tabs) (when rest (list rest)))
                     separator)
      ;; (propertize (buffer-name current-buf) 'face 'ale-tab-current-tab-face))))
      (propertize (concat " " (buffer-name current-buf) " ") 'face 'ale-tab-current-tab-face))))

(defun ale-tab-previous ()
  "Switch to the previous tab.
When the current buffer is the first tab, or not in the tabs,
switch to the last tab."
  (interactive)
  (let* ((buf (current-buffer))
         (tabs (ale-tab-visible-tabs))
         (idx (cl-position buf tabs :test #'eq))
         (ale-tab/inhibit-resort t))
    (cond
     ((or (null idx) (eq idx 0))
      (switch-to-buffer (car (last tabs))))
     (t
      (switch-to-buffer (nth (1- idx) tabs))))))

(defun ale-tab-next ()
  "Switch to the next tab.
When the current buffer is the last tab, or not in the tabs,
switch to the first tab."
  (interactive)
  (let* ((buf (current-buffer))
         (tabs (ale-tab-visible-tabs))
         (idx (cl-position buf tabs :test #'eq))
         (ale-tab/inhibit-resort t))
    (cond
     ((or (null idx) (eq idx (1- (length tabs))))
      (switch-to-buffer (car tabs)))
     (t
      (switch-to-buffer (nth (1+ idx) tabs))))))

(defun ale-tab-kill-invisible-buffers-in-group ()
  "Kill all buffers that's not in the tabs in current group.
Notice when the current buffer is not in the tabs, though it may
still be shown after the \"n+\" part in the tabs, it will be
killed."
  (interactive)
  (when-let ((group (ale-tab/buffer-group (current-buffer)))
             (tabs (ale-tab-visible-tabs)))
    (dolist (b (buffer-list))
      (when (and (equal (ale-tab/buffer-group b) group)
                 (not (memq b tabs)))
        (kill-buffer b)))))

(defun ale-tab-kill-buffers-in-group ()
  "Kill all buffers in current group."
  (interactive)
  (when-let ((group (ale-tab/buffer-group (current-buffer))))
    (dolist (b (buffer-list))
      (when (equal (ale-tab/buffer-group b) group)
        (kill-buffer b)))))

(defun ale-tab-switch-to-buffer-in-group ()
  "Switch to a buffer in current group."
  (interactive)
  (when-let ((group (ale-tab/buffer-group (current-buffer))))
    (let (bufs collection)
      (dolist (b (buffer-list))
        (when (equal (ale-tab/buffer-group b) group)
          (push b bufs)))
      (setq bufs (mapcar #'buffer-name (nreverse bufs)))
      (setq collection
            (lambda (str pred action)
              (if (eq action 'metadata)
                  '(metadata
                    (category . buffer)
                    (cycle-sort-function . identity)
                    (display-sort-function . identity))
                (complete-with-action action bufs str pred))))
      (switch-to-buffer
       (completing-read "Switch to: " collection nil t)))))

;;;###autoload
(define-minor-mode ale-tab-mode
  "Minor mode for maintaining data for showing tabs.
This mode doesn't offer an UI for showing the tabs.  See
`ale-tab-update-hook' and `ale-tab-string' to know how to
plug-in an UI for tabs."
  :global t
  (if ale-tab-mode
      (progn
        (ale-tab-start-count-freq)
        (ale-tab/update-buffer-list)
        (add-hook 'buffer-list-update-hook
                  #'ale-tab/update-buffer-list))
    (ale-tab-stop-count-freq)
    (remove-hook 'buffer-list-update-hook
                 #'ale-tab/update-buffer-list)))

(provide 'ale-tab)

;;; autoload/tab.el ends here
