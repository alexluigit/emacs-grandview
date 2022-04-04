;;; early-init.el --- -*- lexical-binding: t -*-

;; Defer GC (see `autoload/gc.el')
(setq gc-cons-threshold most-positive-fixnum)

;; Setup native compilation
(add-to-list 'native-comp-eln-load-path "~/.cache/emacs/eln-cache/")
(setq native-comp-async-report-warnings-errors 'silent)

(setq package-quickstart nil)
(setq package-enable-at-startup nil)  ; Prevent package.el loading packages prior to their init-file

;; Disable warnings from legacy advice system. They aren't useful, and what can
;; we do about them, besides changing packages upstream?
(setq ad-redefinition-action 'accept)

;; Reduce *Message* noise at startup. An empty scratch buffer (or the dashboard)
;; is more than enough.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-screen t
      inhibit-default-init t
      initial-scratch-message nil)

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (don't rely on case insensitivity for file names).
(setq auto-mode-case-fold nil)

;; Disable bidirectional text scanning for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling the BPA makes redisplay faster, but might produce incorrect display
;; reordering of bidirectional text with embedded parentheses and other bracket
;; characters whose 'paired-bracket' Unicode property is non-nil.
(setq bidi-inhibit-bpa t)  ; Emacs 27 only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it there anyway, just in case. This increases
;; memory usage, however!
(setq inhibit-compacting-font-caches t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 1024 1024))  ; 1mb

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(setq command-line-x-option-alist nil)

;; Emacs hangs when large selections contain mixed line endings.
;; This problem is described in etc/PROBLEMS
;; See also: <https://gnu.emacs.bug.narkive.com/Kbl5Gryo/bug-16737-timed-out-waiting-for-reply-from-selection-owner>
(setq select-active-regions 'only)

;; Disable backup files
(setq auto-save-list-file-prefix nil
      auto-save-default nil
      make-backup-files nil
      create-lockfiles nil)

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

;; Do not display continuation lines
(setq-default truncate-lines t)

;; Sentence settings
(setq sentence-end-double-space t)
(setq sentence-end-without-period nil)

;; y,n for yes,no
(if (<= emacs-major-version 28)
    (defalias 'yes-or-no-p 'y-or-n-p)
  (setq use-short-answers t))

(tool-bar-mode -1)                    ; Disable toolbar
(tooltip-mode -1)                     ; Disable tooltips
(menu-bar-mode -1)                    ; Disable menu bar
(scroll-bar-mode -1)                  ; Disable scroll bar
