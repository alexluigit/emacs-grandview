;;; autoload/org-id.el -*- lexical-binding: t; -*-

(require 'org-id)

(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(defun ale-org-id-new (&optional prefix)
  "Create a new globally unique ID.

An ID consists of two parts separated by a colon:
- a prefix
- a unique part that will be created according to `org-id-method'.

PREFIX can specify the prefix, the default is given by the
variable `org-id-prefix'.  However, if PREFIX is the symbol
`none', don't use any prefix even if `org-id-prefix' specifies
one. So a typical ID could look like \"Org-4nd91V40HI\"."
  (let* ((prefix (if (eq prefix 'none)
                     ""
                   (concat (or prefix org-id-prefix) "-")))
         unique)
    (when (equal prefix "-") (setq prefix ""))
    (cond
     ((memq org-id-method
            '(uuidgen uuid))
      (setq unique (org-trim (shell-command-to-string org-id-uuid-program)))
      (unless (org-uuidgen-p unique)
        (setq unique (org-id-uuid))))
     ((eq org-id-method 'org)
      (let* ((etime (org-reverse-string (org-id-time-to-b36)))
             (postfix (when org-id-include-domain
                        (require 'message)
                        (concat "@"
                                (message-make-fqdn)))))
        (setq unique (concat etime postfix))))
     (t (error "Invalid `org-id-method'")))
    (concat prefix (car (split-string unique "-")))))

;;;###autoload
(defun ale-org-custom-id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.

If POM is nil, refer to the entry at point. If the entry does not
have an CUSTOM_ID, the function returns nil. However, when CREATE
is non nil, create a CUSTOM_ID if none is present already. PREFIX
will be passed through to `ale-org-id-new'. In any case, the
CUSTOM_ID of the entry is returned."
  (interactive)
  (org-with-point-at pom
    (let* ((orgpath (mapconcat #'identity (org-get-outline-path) "-"))
           (heading (replace-regexp-in-string
                     "/\\|~\\|\\[\\|\\]" ""
                     (replace-regexp-in-string
                      "[[:space:]]+" "_" (if (string= orgpath "")
                                             (org-get-heading t t t t)
                                           (concat orgpath "-" (org-get-heading t t t t))))))
           (id (org-entry-get nil "CUSTOM_ID")))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create (setq id (ale-org-id-new (concat prefix heading)))
               (org-entry-put pom "CUSTOM_ID" id)
               (org-id-add-location id
                                    (buffer-file-name (buffer-base-buffer)))
               id)))))

;;;###autoload
(defun ale-org-add-ids-to-headlines-in-file (&optional force)
  "Add CUSTOM_ID properties to all headlines in the current file
which do not already have one.

Only adds ids if the `auto-id' option is set to `t' in the file
somewhere. ie, #+OPTIONS: auto-id:t"
  (interactive "P")
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+OPTIONS:.*auto-id:t" (point-max) t)
      (when force
        (org-map-entries (lambda () (org-entry-delete nil "CUSTOM_ID"))))
      (org-map-entries (lambda () (ale-org-custom-id-get (point) 'create))))))

;;;###autoload
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda ()
                        (when (and (eq major-mode 'org-mode)
                                   (eq buffer-read-only nil))
                          (ale-org-add-ids-to-headlines-in-file))))))
