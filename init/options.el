;;; init/options.el --- -*- lexical-binding: t -*-

;; Disable backup files
(setq auto-save-list-file-prefix nil
      auto-save-default nil
      make-backup-files nil
      create-lockfiles nil)

;; No littering
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))

;; Don't ask for whether to follow symlink or not
(setq vc-follow-symlinks nil)

;; By default, page scrolling should keep the point at the same visual
;; position, rather than force it to the top or bottom of the
;; viewport.  This eliminates the friction of guessing where the point
;; has warped to.

;; As for per-line scrolling, I dislike the default behaviour of
;; visually re-centring the point: it is too aggressive as a standard
;; mode of interaction.  With the following =setq-default=, the point
;; will stay at the top/bottom of the screen while moving in that
;; direction (use C-l to reposition it).
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq hscroll-step 1)
(setq hscroll-margin 1)

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
(put 'dired-find-alternate-file 'disabled nil)
(setq custom-file (concat user-emacs-directory "custom.el"))

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
(setq-default tab-always-indent t)
(setq-default tab-first-completion 'word-or-paren-or-punct) ; Emacs 27
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Show current key strokes in echo area after 0.25s
(setq echo-keystrokes 0.25)

;; y,n for yes,no
(if (version< emacs-version "28")
    (defalias 'yes-or-no-p 'y-or-n-p)
  (setq use-short-answers t))
