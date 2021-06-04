;;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'project)
(require 'vc)
(require 'ace-completing)

(defgroup ace/project ()
  "Extensions for project.el and related libraries."
  :group 'project)

(defcustom ace/project-project-roots (list "~/Dev")
  "List of directories with version-controlled projects.
To be used by `ace/project-switch-project'."
  :type 'list
  :group 'ace/project)

(defcustom ace/project-commit-log-limit 25
  "Limit commit logs for project to N entries by default.
A value of 0 means 'unlimited'."
  :type 'integer
  :group 'ace/project)

(defcustom ace/project-large-file-lines 1000
  "How many lines constitute a 'large file' (integer).
This determines whether some automatic checks should be executed
or not, such as `ace/project-flymake-mode-activate'."
  :type 'integer
  :group 'ace/project)

(cl-defmethod project-root ((project (head local)))
  "Project root for PROJECT with HEAD and LOCAL."
  (cdr project))

;; Copied from Manuel Uberti and tweaked accordingly:
;; <https://www.manueluberti.eu/emacs/2020/11/14/extending-project/>.
(defun ace/project--project-files-in-directory (dir)
  "Use `fd' to list files in DIR."
  (unless (executable-find "fd")
    (error "Cannot find 'fd' command is shell environment $PATH"))
  (let* ((default-directory dir)
         (localdir (file-local-name (expand-file-name dir)))
         (command (format "fd -t f -0 . %s" localdir)))
    (project--remote-file-names
     (split-string (shell-command-to-string command) "\0" t))))

(cl-defmethod project-files ((project (head vc)) &optional dirs)
  "Override `project-files' to use `fd' in local projects.
Project root for PROJECT with HEAD and VC, plus optional
DIRS."
  (mapcan #'ace/project--project-files-in-directory
          (or dirs (list (project-root project)))))

(defun ace/project--list-projects ()
  "Produce list of projects in `ace/project-project-roots'."
  (let* ((dirs ace/project-project-roots)
         (dotless directory-files-no-dot-files-regexp)
         (cands (mapcan (lambda (d)
                          (directory-files d t dotless))
                        dirs)))
    (mapcar (lambda (d)
              (list (abbreviate-file-name d)))
            cands)))

;; ;;;###autoload
;; (defun ace/project-fd-other-window ()
;;   "Same as `project-find-file', but open file in other window

;; The completion default is the filename at point, determined by
;; `thing-at-point' (whether such file exists or not)."
;;   (interactive)
;;   (let* ((pr (project-current t))
;;          (dirs (list (project-root pr)))
;;          (all-files (project-files pr dirs))
;;          (completion-ignore-case read-file-name-completion-ignore-case)
;;          (filename (thing-at-point 'filename))
;;          (file (funcall project-read-file-name-function
;;                                  "Find file" all-files nil nil
;;                                  filename)))
;;   (if (string= file "")
;;           (user-error "You didn't specify the file")
;;         (find-file-other-window file))))

;;;###autoload
(defun ace/project-add-projects ()
  "Append `ace/project--list-projects' to `project--list'."
  (interactive)
  (project--ensure-read-project-list)
  (let ((projects (ace/project--list-projects)))
    (setq project--list (append projects project--list))
    (project--write-project-list)))

;;;###autoload
(defun ace/project-remove-project ()
  "Remove project from `project--list' using completion."
  (interactive)
  (project--ensure-read-project-list)
  (let* ((projects project--list)
         (dir (completing-read "REMOVE project from list: " projects nil t)))
    (setq project--list (delete (assoc dir projects) projects))
    (project--write-project-list)))

(defun ace/project--directory-subdirs (dir)
  "Return list of subdirectories in DIR."
  (cl-remove-if (lambda (x) (string-match-p "\\.git" x))
    (cl-remove-if-not (lambda (x) (file-directory-p x))
      (directory-files-recursively dir ".*" t t))))

;;;###autoload
(defun ace/project-find-subdir ()
  "Find subdirectories in the current project, using completion."
  (interactive)
  (let* ((pr (project-current t))
         (dir (cdr pr))
         (dirs-raw (ace/project--directory-subdirs dir))
         (subdirs (ace/completing-append-metadata 'file dirs-raw))
         (directory (completing-read "Select Project subdir: " subdirs)))
    (dired directory)))

;;;###autoload
(defun ace/project-commit-log (&optional arg)
  "Print commit log for the current project.
With optional prefix ARG (\\[universal-argument]) shows expanded
commit messages and corresponding diffs.

The log is limited to the integer specified by
`ace/project-commit-log-limit'.  A value of 0 means
'unlimited'."
  (interactive "P")
  (let* ((pr (project-current t))
         (dir (cdr pr))
         (default-directory dir) ; otherwise fails at spontaneous M-x calls
         (backend (vc-responsible-backend dir))
         (num ace/project-commit-log-limit)
         (int (if (numberp num) num (error "%s is not a number" n)))
         (limit (if (= int 0) t int))
         (diffs (if arg 'with-diff nil))
         (vc-log-short-style (unless diffs '(directory))))
    (vc-print-log-internal backend (list dir) nil nil limit diffs)))

;;;###autoload
(defun ace/project-retrieve-tag ()
  "Run `vc-retrieve-tag' on project and switch to the root dir.
Basically switches to a new branch or tag."
  (interactive)
  (let* ((pr (project-current t))
         (dir (cdr pr))
         (default-directory dir) ; otherwise fails at spontaneous M-x calls
         (name
          (vc-read-revision "Tag name: "
                            (list dir)
                            (vc-responsible-backend dir))))
    (vc-retrieve-tag dir name)
    (project-dired)))

(autoload 'magit-status "magit")

;;;###autoload
(defun ace/project-magit-status ()
  "Run `magit-status' on project."
  (interactive)
  (let* ((pr (project-current t))
         (dir (cdr pr)))
    (magit-status dir)))

(provide 'ace-project)
