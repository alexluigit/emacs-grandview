;;; core-editor.el --- -*- lexical-binding: t -*-

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

;; Keep a record of all recently opened files.
(use-package recentf
  :straight (:type built-in)
  :after-call find-file-hook danger
  :config
  (add-to-list 'recentf-exclude (lambda (f) (not (string= (file-truename f) f))))
  (recentf-mode 1))

;; Just remember where the point is in any given file.  This can often
;; be a subtle reminder of what you were doing the last time you
;; visited that file, allowing you to pick up from there.
(use-package saveplace
  :straight (:type built-in)
  :after-call find-file-hook
  :config
  (setq save-place-file (locate-user-emacs-file "saveplace"))
  (setq save-place-forget-unreadable-files t)
  (save-place-mode 1))

;; This mode ensures that the buffer is updated whenever the file
;; changes.  A change can happen externally or by some other tool
;; inside of Emacs (e.g. kill a Magit diff).
(use-package autorevert
  :straight (:type built-in)
  :config
  (setq auto-revert-verbose t)
  :hook
  (after-init . global-auto-revert-mode))

;; Display line numbers when programming
(use-package display-line-numbers
  :straight (:type built-in)
  :hook
  (prog-mode . display-line-numbers-mode))

;; Jump list
(use-package better-jumper
  :after-call pre-command-hook
  :config
  (better-jumper-mode +1)
  (ale-jumper-sensible-jump-mode))

;; Jump to visible text
(use-package avy
  :config
  (setq avy-timeout-seconds 0.3)
  (setq avy-all-windows nil)
  (setq avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o)))

;; Emacs labels as `electric' any behaviour that involves contextual auto-insertion
;; of characters.

;; - Indent automatically.

;; - If =electric-pair-mode= is enabled (which I might do manually),
;;   insert quotes and brackets in pairs.  Only do so if there is no
;;   alphabetic character after the cursor.

;; - To get those numbers, evaluate =(string-to-char CHAR)= where CHAR
;;   is the one you are interested in.  For example, get the literal
;;   tab's character with `(string-to-char "\t")'.

;; - While inputting a pair, inserting the closing character will just
;;   skip over the existing one, rather than add a new one.

;; - Do not skip over whitespace when operating on pairs.  Combined
;;   with the above point, this means that a new character will be
;;   inserted, rather than be skipped over.  I find this better,
;;   because it prevents the point from jumping forward, plus it
;;   allows for more natural editing.

;; - The rest concern the conditions for transforming quotes into
;;   their curly equivalents.  I keep this disabled, because curly
;;   quotes are distinct characters.  It is difficult to search for
;;   them.  Just note that on GNU/Linux you can type them directly by
;;   hitting the "compose" key and then an angled bracket (=<= or =>=)
;;   followed by a quote mark.
(use-package electric
  :config
  (advice-add 'electric-pair-post-self-insert-function :around
              (lambda (fn &rest args) (let ((mark-active nil)) (apply fn args))))
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (setq electric-pair-preserve-balance t)
  (setq electric-pair-pairs
        '((8216 . 8217)
          (8220 . 8221)
          (171 . 187)))
  (setq electric-pair-skip-self 'electric-pair-default-skip-self)
  (setq electric-pair-skip-whitespace nil)
  (setq electric-pair-skip-whitespace-chars '(9 10 32))
  (setq electric-quote-context-sensitive t)
  (setq electric-quote-paragraph t)
  (setq electric-quote-string nil)
  (setq electric-quote-replace-double t)
  (electric-indent-mode 1)
  (electric-pair-mode 1)
  (electric-quote-mode -1)
  :hook
  (org-mode . ale-electric-inhibit-<)
  (minibuffer-setup . (lambda () (unless (eq this-command 'eval-expression) (electric-pair-mode 0))))
  (minibuffer-exit . (lambda () (electric-pair-mode 1))))

;; Pairs insert/change/delete
;; I've forked this package to extract `embrace-default-pairs' out.
(use-package embrace
  :straight
  (embrace :type git :depth full :host github
           :repo "cute-jumper/embrace.el"
           :fork (:host github :repo "alexluigit/embrace.el"))
  :init
  (setq embrace-default-pairs '((?r . ("(" . ")"))
                                (?R . ("( " . " )"))
                                (?c . ("{" . "}"))
                                (?C . ("{ " . " }"))
                                (?\[ . ("[" . "]"))
                                (?\] . ("[ " . " ]"))
                                (?a . ("<" . ">"))
                                (?A . ("< " . " >"))
                                (?s . ("\"" . "\""))
                                (?\' . ("\'" . "\'"))
                                (?` . ("`" . "`")))))

;; I believe tabs, in the sense of inserting the tab character, are
;; best suited for indentation.  While spaces are superior at
;; precisely aligning text.  However, I understand that elisp uses its
;; own approach, which I do not want to interfere with.  Also, Emacs
;; tends to perform alignments by mixing tabs with spaces, which /can
;; actually lead to misalignments/ depending on certain variables such
;; as the size of the tab.  As such, I am disabling tabs by default.

;; If there ever is a need to use different settings in other modes,
;; we can customise them via hooks.  This is not an issue I have
;; encountered yet and am therefore refraining from solving a problem
;; that does not affect me.

;; Note that `tab-always-indent' will first do indentation and then
;; try to complete whatever you have typed in.
(setq-default tab-always-indent 'complete)
(setq-default tab-first-completion 'word-or-paren-or-punct) ; Emacs 27
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; With `wgrep' we can directly edit the results of a grep and save
;; the changes to all affected buffers.  In principle, this is the
;; same as what the built-in occur offers.  We can use it to operate
;; on a list of matches by leveraging the full power of Emacs' editing
;; capabilities (e.g. keyboard macros, query and replace a regexp...).
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  :bind
  (:map wgrep-mode-map
        ("M-n" . next-error-no-select)
        ("M-p" . previous-error-no-select)))

;; Minibuffer query string input
(use-package isearch-mb
  :init
  (isearch-mb-mode)
  :config
  (add-to-list 'isearch-mb--with-buffer #'consult-isearch)
  (add-to-list 'isearch-mb--after-exit #'anzu-isearch-query-replace)
  :bind
  (:map isearch-mb-minibuffer-map
        ([remap previous-matching-history-element] . consult-isearch)))

;; Use M-n and M-p for navigating between paragraphs.
(use-package paragraphs
  :straight (:type built-in)
  :bind
  ("M-n" . forward-paragraph)
  ("M-p" . backward-paragraph)
  :init
  (setq-default truncate-lines t)
  :config
  (setq ale/fill-default-column 80)
  (setq ale/fill-prog-mode-column 80)  ; Set this to another value if you want
  (setq sentence-end-double-space t)
  (setq sentence-end-without-period nil)
  (setq colon-double-space nil)
  (setq use-hard-newlines nil)
  (setq adaptive-fill-mode t)
  (ale/fill-fill-mode 1))

;; Xref provides helpful commands for code navigation and discovery,
;; such as `xref-find-definitions' (M-.) and its counterpart
;; `xref-pop-marker-stack' (M-,).  It is a library that gets used by a
;; variety of tools, including `project.el'.
(use-package xref
  :straight (:type built-in)
  :config
  ;; All those have been changed for Emacs 28
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)
  (setq xref-file-name-display 'project-relative)
  (setq xref-search-program 'ripgrep))

;; Prevent files with extremely long line freeze emacs
(use-package so-long
  :straight (:type built-in)
  :config
  (setq so-long-threshold 400)
  :hook
  (emacs-startup . global-so-long-mode))

;; When you install a package or use the various customisation
;; interfaces to tweak things to your liking, Emacs will append a
;; piece of Elisp to your init file. In my experience, this is a
;; common source of inconsistencies, arising from a conflict between
;; the user's code and what is stored in that custom snippet.

;; As it does not seem possible to outright disable this behaviour, I
;; instruct Emacs to place all "custom" code in a temporary file that
;; never gets loaded. This feels kinda hacky but is better than having
;; some arbitrary code that you accidentally evaluated from messing up
;; with your carefully designed (and version-controlled)
;; configuration.
(put 'list-timers 'disabled nil)
(put 'erase-buffer 'disabled nil)
(setq custom-file (concat user-emacs-directory "ale-custom.el"))

(provide 'core-editor)
