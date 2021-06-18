(require 'eshell)
(require 'esh-mode)
(eval-when-compile (require 'subr-x))
(require 'cl-seq)
(declare-function ace/files-read "ace-files")

(defcustom ace/eshell-position '((window-width . 0.4) (side . right))
  "doc")

(defvar ace/eshell-buffers nil
  "The list of non-dedicated eshell buffers.")

(defvar ace/eshell-index 0
  "The list of non-dedicated eshell buffers.")

(defun ace/get-current-package-version ()
  (interactive)
  (let ((package-json-file (concat (eshell/pwd) "/package.json")))
    (when (file-exists-p package-json-file)
      (let* ((package-json-contents (ace/files-read package-json-file))
             (package-json (ignore-errors (json-parse-string package-json-contents))))
        (when package-json
          (ignore-errors (gethash "version" package-json)))))))

(defun ace/map-line-to-status-char (line)
  (cond ((string-match "^?\\? " line) "?")))

(defun ace/get-git-status-prompt ()
  (let ((status-lines (cdr (process-lines "git" "status" "--porcelain" "-b"))))
    (seq-uniq (seq-filter 'identity (mapcar 'ace/map-line-to-status-char status-lines)))))

(defun ace/get-prompt-path ()
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
        (abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))

;; This prompt function mostly replicates my custom zsh prompt setup
;; that is powered by github.com/denysdovhan/spaceship-prompt.
(defun ace/eshell-prompt ()
  (let* ((br-cmd "git symbolic-ref HEAD 2>/dev/null || git rev-parse --short HEAD 2>/dev/null")
         (br-raw (shell-command-to-string br-cmd))
         (current-branch (replace-regexp-in-string "\\(refs/heads/\\)\\|\\(\n\\)$" "" br-raw))
         (shell-index (number-to-string (ace/eshell-get--index (current-buffer))))
         (package-version (ace/get-current-package-version)))
    (concat
     (if (= (user-uid) 0)
         (propertize (concat "S-" shell-index) 'face `(:foreground "red2"))
       (propertize (concat "S-" shell-index) 'face `(:foreground "#62aeed")))
     (propertize " • " 'face `(:foreground "white"))
     (propertize "  " 'face `(:foreground "#82cfd3"))
     (propertize (ace/get-prompt-path) 'face `(:foreground "#82cfd3"))
     (when (not (string= current-branch ""))
       (concat
        (propertize " • " 'face `(:foreground "white"))
        (propertize (concat " " current-branch) 'face `(:foreground "#ab98b5"))))
     (when package-version
       (concat
        (propertize " @ " 'face `(:foreground "white"))
        (propertize package-version 'face `(:foreground "#e8a206"))))
     (propertize " • " 'face `(:foreground "white"))
     (propertize (format-time-string "%I:%M:%S %p") 'face `(:foreground "#5a5b7f"))
     (if (= eshell-last-command-status 0)
         (propertize "\nλ" 'face `(:foreground "#ADCF44"))
       (propertize "\nλ" 'face `(:foreground "#EC6261")))
     (propertize " " 'face `(:foreground "white")))))

(defun ace/eshell-configure ()
  (push 'eshell-tramp eshell-modules-list)
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  ;; Use completion-at-point to provide completions in eshell
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
  ;; Initialize the shell history
  (eshell-hist-initialize)
  (setenv "PAGER" "cat")
  (setq eshell-prompt-function      'ace/eshell-prompt
        eshell-prompt-regexp        "^λ "
        eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-highlight-prompt t
        eshell-scroll-to-bottom-on-input t
        eshell-prefer-lisp-functions nil))

(add-hook 'eshell-exit-hook
          (lambda () (setq ace/eshell-buffers (delq (current-buffer) ace/eshell-buffers))))

(defun ace/eshell-toggle (&optional force-new)
  "Toggle eshell.
If called with prefix argument, create a new eshell buffer if
current one have different `default-directory'."
  (interactive "P")
  (if (eq major-mode 'eshell-mode)
      (delete-window)
    (let* ((display-buffer-alist
            `(("^\\*[e]shell.*"
               (display-buffer-in-side-window)
               (window-parameters . ((mode-line-format . none)))
               ,@ace/eshell-position)))
           (buf (nth ace/eshell-index ace/eshell-buffers))
           (dir (expand-file-name default-directory))
           (index (if buf (ace/eshell-get--index buf) 0)))
      (add-to-list 'ace/eshell-buffers (eshell index))
      (when force-new
        (unless (string= dir (expand-file-name default-directory))
          (let ((default-directory dir))
            (ace/eshell-new))))
      (when (featurep 'evil) (evil-insert-state))
      (when (featurep 'meow) (meow-insert)))))

(defun ace/eshell-get--index (buf)
  (let* ((name (buffer-name buf)))
    (string-match "\\*eshell\\*\<\\([0-9]+\\)\>" name)
    (string-to-number (cl-subseq name (match-beginning 1) (match-end 1)))))

(defun ace/eshell-new ()
  "Create new eshell buffer."
  (interactive)
  (let ((new-index (1+ (ace/eshell-get--index (car ace/eshell-buffers))))
        (display-buffer-alist
         `(("^\\*[e]shell.*"
            (display-buffer-in-side-window)
            (window-parameters . ((mode-line-format . none)))
            ,@ace/eshell-position))))
    (add-to-list 'ace/eshell-buffers (eshell new-index))))

(defun ace/eshell-next (&optional arg)
  "Select next eshell buffer.
Create new one if no eshell buffer exists."
  (interactive "P")
  (let* ((curr-index (cl-position (current-buffer) ace/eshell-buffers))
         (new-index (+ curr-index (or arg -1)))
         (buf (nth new-index ace/eshell-buffers)))
    (when buf
      (switch-to-buffer buf)
      (setq ace/eshell-index new-index))))

(defun ace/eshell-prev (&optional arg)
  "Select previous eshell buffer."
  (interactive "p")
  (ace/eshell-next arg))

(defun ace/eshell-clear-buffer ()
  "Clear eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input nil nil t)))

(defun ace/eshell-pacman-install ()
  "Choose a package to install using paru."
  (interactive)
  (let* ((p-list-raw (ace/files-read "/home/alex/.local/share/paru/pkglist"))
         (p-list (split-string p-list-raw "\n" t))
         (res (completing-read "Install: " p-list)))
    (insert (concat "paru " res))))

(defun ace/eshell-pacman-uninstall ()
  "Choose a package to uninstall using paru."
  (let* ((p-list-raw (shell-command-to-string "pacman -Qeq"))
         (p-list (split-string p-list-raw "\n" t))
         (res (completing-read "Uninstall: " p-list)))
    (insert (concat "paru -Rns " res))))

(provide 'ace-eshell)