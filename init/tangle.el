;;; init/tangle.el --- -*- lexical-binding: t -*-

(defun ale-tangle--all (&optional force)
  "doc"
  (let* ((init-md5 (concat ale-cache-dir "init.md5"))
         (old-md5 (when (file-exists-p init-md5)
                    (file-read! init-md5)))
         (new-md5 (secure-hash 'md5 (file-read! ale-full-config-org))))
    (when (or force (not (string= old-md5 new-md5)))
      (when (file-exists-p (ale-minimal-config))
        (delete-file (ale-minimal-config)))
      (when (file-exists-p ale-full-config)
        (delete-file ale-full-config))
      (with-temp-buffer
        (erase-buffer)
        (insert new-md5)
        (write-region (point-min) (point-max) init-md5))
      (require 'ob-tangle)
      (org-babel-tangle-file ale-full-config-org ale-full-config))))

(defun ale-tangle--gen-autoload (&optional force)
  "Generate core autoload files."
  (require 'autoload)
  (let* ((autoload-md5 (concat ale-cache-dir "autoload.md5"))
         (old-md5 (when (file-exists-p autoload-md5)
                    (file-read! autoload-md5)))
         (files-as-str (with-temp-buffer
                         (dolist (file (ale-files-get-all-elisp ale-autoload-dirs))
                           (insert-file-contents file))
                         (buffer-string)))
         (new-md5 (secure-hash 'md5 files-as-str)))
    (when (or force (not (string= old-md5 new-md5)))
      (with-temp-file ale-autoload-file
        (cl-loop with generated-autoload-file = nil
                 with inhibit-message = t
                 for file in (ale-files-get-all-elisp ale-autoload-dirs)
                 for generated-autoload-load-name = (file-name-sans-extension file)
                 do (autoload-generate-file-autoloads file (current-buffer))))
      (with-temp-buffer
        (erase-buffer)
        (insert new-md5)
        (write-region (point-min) (point-max) autoload-md5)))))

(defun ale-tangle (&optional force)
  (ale-tangle--prepare-heading)
  (ale-tangle--all force)
  (ale-tangle--gen-autoload force))

(defun ale-tangle--gen-path-for-autoload (pom)
  (org-with-point-at pom
    (when (string= (org-get-heading) "Autoload")
      (let* ((package-name
              (save-excursion
                (org-up-heading-safe)
                (let ((heading (org-get-heading)))
                  (substring-no-properties heading (1+ (string-match "(\\(.*\\))" heading)) (match-end 1)))))
             (tangle-path (concat ":tangle \"" ale-autoload-default-dir package-name "\"")))
        (org-entry-put pom "header-args:emacs-lisp" tangle-path)))))

(defun ale-tangle--prepare-heading ()
  (with-current-buffer (find-file-noselect ale-full-config-org)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (org-map-entries (lambda () (ale-tangle--gen-path-for-autoload (point)))))
    (save-buffer)
    (kill-this-buffer)))

(add-hook 'kill-emacs-hook #'ale-tangle)

(unless (file-exists-p ale-cache-dir)
  (make-directory ale-autoload-default-dir t)
  (ale-tangle t))
