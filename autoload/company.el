;;; autoload/company.el --- Extension for company.el -*- lexical-binding: t -*-

(defvar ale-company-disabled-modes '(org-mode))

;;;###autoload
(defun ale-company-enable-ispell (&optional force)
  "Add `company-ispell' to local `company-backends'."
  (interactive "P")
  (require 'company-ispell)
  (when (company-ispell-available)
    (when (or force
              (not (memq major-mode ale-company-disabled-modes)))
      (make-local-variable 'company-backends)
      (if (memq 'company-ispell company-backends)
          (setq-local company-backends (delq 'company-ispell company-backends))
        (add-to-list 'company-backends 'company-ispell)))))

(provide 'ale-company)
