;;; early-init.el --- -*- lexical-binding: t -*-

(setq
 ;; Inhibit garbage collection during startup
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 0.6
 ;; Setup native compilation
 native-comp-eln-load-path
 (or (and (getenv "EMACSNATIVELOADPATH") native-comp-eln-load-path)
     (append (list (expand-file-name "~/.cache/emacs/eln-cache/"))
             (delete (expand-file-name "eln-cache/" user-emacs-directory)
                     native-comp-eln-load-path)))
 native-comp-async-report-warnings-errors 'silent
 ;; Prevent package.el loading packages prior to their init-file
 package-quickstart nil
 package-enable-at-startup nil
 ;; Disable warnings from legacy advice system. They aren't useful, and what can
 ;; we do about them, besides changing packages upstream?
 ad-redefinition-action 'accept
 ;; Reduce noise at startup. An empty scratch buffer is more than enough.
 inhibit-startup-message t
 inhibit-startup-echo-area-message user-login-name
 inhibit-startup-screen t
 inhibit-default-init t
 initial-scratch-message nil
 ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
 ;; indicates misconfiguration (don't rely on case insensitivity for file names).
 auto-mode-case-fold nil
 ;; More performant rapid scrolling over unfontified regions. May cause brief
 ;; spells of inaccurate syntax highlighting right after scrolling, which should
 ;; quickly self-correct.
 fast-but-imprecise-scrolling t
 ;; Don't ping things that look like domain names.
 ffap-machine-p-known 'reject
 ;; Resizing the Emacs frame can be a terribly expensive part of changing the
 ;; font. By inhibiting this, we halve startup times, particularly when we use
 ;; fonts that are larger than the system default (which would resize the frame).
 frame-inhibit-implied-resize t
 ;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
 idle-update-delay 1.0  ; default is 0.5
 ;; Font compacting can be terribly expensive, especially for rendering icon
 ;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
 ;; hasn't been determined, but do it there anyway, just in case. This increases
 ;; memory usage, however!
 inhibit-compacting-font-caches t
 ;; Increase how much is read from processes in a single chunk (default is 4kb).
 ;; This is further increased elsewhere, where needed (like our LSP module).
 read-process-output-max (* 1024 1024)  ; 1mb
 ;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
 ;; receiving input, which should help a little with scrolling performance.
 redisplay-skip-fontification-on-input t
 ;; Remove command line options that aren't relevant to our current OS; means
 ;; slightly less to process at startup.
 command-line-x-option-alist nil
 ;; Emacs hangs when large selections contain mixed line endings.
 ;; This problem is described in etc/PROBLEMS
 ;; See also: <https://gnu.emacs.bug.narkive.com/Kbl5Gryo/bug-16737-timed-out-waiting-for-reply-from-selection-owner>
 select-active-regions 'only
 ;; Disable auto-save/lockfiles
 auto-save-list-file-prefix nil
 create-lockfiles nil
 ;; Do not ask about symlink following
 vc-follow-symlinks t
 ;; No littering
 user-emacs-directory (expand-file-name "~/.cache/emacs/")
 ;; When you install a package or use the various customisation
 ;; interfaces to tweak things to your liking, Emacs will append a
 ;; piece of Elisp to your init file. In my experience, this is a
 ;; common source of inconsistencies, arising from a conflict between
 ;; the user's code and what is stored in that custom snippet.
 ;; As it does not seem possible to outright disable this behaviour, I
 ;; instruct Emacs to place all "custom" code in a temporary file that
 ;; never gets loaded.
 custom-file (concat user-emacs-directory "custom.el")
 ;; y,n for yes,no, this option is added in Emacs 28.1
 use-short-answers t
 ;; Inhibit eval confirmation for `grandview-setup-literate-file'.
 safe-local-variable-values
 '((eval . (ignore-errors (grandview-setup-literate-file)))))

(tool-bar-mode -1)                    ; Disable toolbar
(tooltip-mode -1)                     ; Disable tooltips
(menu-bar-mode -1)                    ; Disable menu bar
(scroll-bar-mode -1)                  ; Disable scroll bar
