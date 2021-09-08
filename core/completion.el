;;; core/completino.el --- -*- lexical-binding: t -*-

;;; Commentary

;; The optimal way of using Emacs is through searching and narrowing
;; selection candidates.  Spend less time worrying about where things
;; are on the screen and more on how fast you can bring them into
;; focus.  This is, of course, a matter of realigning priorities, as
;; we still wish to control every aspect of the interface.

(use-package minibuffer
  :straight (:type built-in)
  :config
  (setq completion-category-defaults nil)
  (setq completion-cycle-threshold 3)
  (setq completion-flex-nospace nil)
  (setq completion-pcm-complete-word-inserts-delimiters t)
  (setq completion-pcm-word-delimiters "-_./:| ")
  (setq completion-show-help nil)
  (setq completion-auto-help nil)
  (setq completion-ignore-case t)
  (setq-default case-fold-search t)   ; For general regexp
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq enable-recursive-minibuffers t)
  (setq read-answer-short t)
  (setq resize-mini-windows 'grow-only)
  (setq minibuffer-eldef-shorten-default t)
  (setq echo-keystrokes 0.25)           ; from the C source code
  (customize-set-variable
   'minibuffer-prompt-properties
   (quote (read-only t cursor-intangible t face minibuffer-prompt)))
  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))

;; Keeps a record of actions involving the minibuffer.  This is of
;; paramount importance to a fast and efficient workflow involving any
;; completion framework that leverages the built-in mechanisms.  Emacs
;; will remember your input and choices and will surface the desired
;; results towards the top as the most likely candidates.
(use-package savehist
  :straight (:type built-in)
  :config
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 10000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  :hook (emacs-startup . savehist-mode))

;; A minimalistic completion UI
(use-package vertico
  :after-call pre-command-hook
  :config
  (set-face-attribute 'vertico-current nil
                      :background
                      (face-attribute 'lazy-highlight :background nil t)
                      :weight 'semi-bold)
  (vertico-mode 1))

;; This package provides an `orderless' completion style that divides
;; the pattern into components (space-separated by default), and
;; matches candidates that match all of the components in any order.
(use-package orderless
  :after-call minibuffer-setup-hook
  :config
  (setq completion-styles '(orderless))
  (setq orderless-component-separator " +")
  (setq orderless-matching-styles
        '(ale-pinyin-build-regexp-string
          orderless-initialism
          orderless-prefixes
          orderless-regexp))
  (setq orderless-style-dispatchers
        '(ale-orderless-literal-dispatcher
          ale-orderless-initialism-dispatcher
          ale-orderless-without-literal-dispatcher
          ale-orderless-pinyin-dispatcher)))

(use-package consult
  :after-call minibuffer-setup-hook
  :init
  (setq completion-in-region-function #'consult-completion-in-region)
  (setq register-preview-delay 0.2)
  (setq register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (advice-add #'switch-to-buffer :around
              (lambda (fn &rest args)
                (interactive)
                (if (car args) (apply fn args) (consult-buffer))))
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  (setq consult-line-numbers-widen t)
  (setq consult-async-min-input 3)
  (setq consult-async-input-debounce 0.5)
  (setq consult-async-input-throttle 0.8)
  (setq consult-narrow-key ">"))

(use-package embark
  :after-call emacs-startup-hook
  :bind
  (("C-." . embark-act)
   :map minibuffer-local-map ("C-." . embark-act) ("C-," . embark-become)
   :map embark-collect-mode-map ("C-." . embark-act))
  :config
  (ale-embark-keymaps 1)
  (setq embark-collect-initial-view-alist
        '((file . list)
          (buffer . list)
          (symbol . list)
          (line . list)
          (xref-location . list)
          (kill-ring . zebra)
          (t . list)))
  (setq embark-quit-after-action t)
  (setq embark-action-indicator
        (let ((act (propertize "Act" 'face 'success)))
          (cons act (concat act " on '%s'"))))
  (setq embark-become-indicator (propertize "Become" 'face 'warning)))

(use-package embark-consult
  :after-call (minibuffer-setup-hook embark-act))

;; This is a utility jointly developed by Daniel Mendler and Omar
;; Antolín Camarena that provides annotations to completion
;; candidates.  It is meant to be framework-agnostic, so it works with
;; Selectrum, Icomplete vertico, and Embark.
(use-package marginalia
  :after-call minibuffer-setup-hook
  :config
  (setq marginalia-annotators
        '(marginalia-annotators-heavy
          marginalia-annotators-light))
  (marginalia-mode))

(use-package company
  :after-call self-insert-command
  :config
  (global-company-mode)
  ;; `tng' means tab and go, in this mode tab key will complete and
  ;; move to the next candidate meanwhile keep company window open.
  (company-tng-mode)
  (setq company-idle-delay 0.0))

(provide 'core-completion)
