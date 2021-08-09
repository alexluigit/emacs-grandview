;;; core-ui.el --- -*- lexical-binding: t -*-

;; icons-library
(use-package all-the-icons)

;; pixelwise (linear or bezier) scrolling in emacs.
(use-package good-scroll
  :after-call find-file-hook
  :config
  (good-scroll-mode 1))

;; interactive query replace
(use-package anzu
  :after-call find-file-hook
  :config
  (global-anzu-mode +1))

;; The `display-buffer-alist' is intended as a rule-set for
;; controlling the display of windows.  The objective is to create a
;; more intuitive workflow where targeted buffer groups or types are
;; always shown in a given location, on the premise that
;; predictability improves usability.

;; For each buffer action in it we can define several functions for
;; selecting the appropriate window.  These are executed in sequence,
;; but my usage thus far suggests that a simpler method is just as
;; effective for my case.

;; Additionally, I've set `split-height-threshold' to nil and
;; `split-width-threshold' to 0 to ensure every new window will open
;; in horizontal split.
(use-package window
  :straight (:type built-in)
  :config
  (setq display-buffer-alist
        `(("\\*\\(Flymake\\|Messages\\|Backtrace\\|Warnings\\|Compile-Log\\|Custom\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.2)
           (side . top))
          ("^\\*?\\(magit: \\|Help\\|helpful\\).*"
           (display-buffer-in-side-window)
           (window-width . 0.4)
           (side . right))
          ("\\*\\vc-\\(incoming\\|outgoing\\|Output\\|Register Preview\\).*"
           (display-buffer-at-bottom))))
  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
  (setq switch-to-buffer-in-dedicated-window 'pop)
  (setq split-height-threshold nil)
  (setq split-width-threshold 0))

;; transient.el is a built-in package in emacs 28.
(use-package transient
  :straight (:type built-in)
  :config
  (setq transient-show-popup -0.5)
  (transient-bind-q-to-quit)
  (define-key transient-map (kbd "<escape>") #'transient-quit-all)
  (define-key transient-sticky-map (kbd "ESC") #'transient-quit-all))

;; Index based window motions
(use-package ace-window
  :bind
  ("M-o" . ace-select-window)
  :config
  (setq aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o)))

(use-package awesome-tab
  :after meow
  :after-call emacs-startup-hook
  :config
  (awesome-tab-mode t)
  (setq awesome-tab-hide-tab-function 'ale-tab-hide-tab)
  (setq awesome-tab-buffer-groups-function 'ale-tab-buffer-groups)
  :custom
  (awesome-tab-ace-keys '(?a ?r ?s ?t ?n ?e ?i ?o ?m))
  :bind
  (("M-i" . awesome-tab-ace-jump)
   :map meow-leader-keymap
   ("[" . awesome-tab-backward-tab)
   ("]" . awesome-tab-forward-tab)))

(unless (or (daemonp) window-system)
  (advice-add 'meow--update-cursor :after #'ale-set-cursor))

(add-to-list 'default-frame-alist '(internal-border-width . 30))
(fringe-mode 1)

;; `window-divider-mode' is a built-in mode that draws vertical window
;; borders in a slightly different way than the default, which I find
;; more consistent.  Only using it because of that, though it can also
;; adjust the size of the borders as well as their placement.
(setq window-divider-default-right-width 10)
(setq window-divider-default-places 'right-only)
(add-hook 'after-init-hook #'window-divider-mode)

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

;; setup fonts, line highlighting
(when (daemonp)
  (add-hook 'after-make-frame-functions
            (lambda (f) (with-selected-frame f
                     (ale-window-recenter-mode)
                     (ale-pulse-advice-commands-mode 1)
                     (ale-font-setup)))))

(add-hook 'after-init-hook 'ale-modeline-mode)

;; modus is a built-in theme in emacs (version >= 28) created by Protesilaos Stavrou.
(setq modus-themes-links 'no-underline)
(add-hook 'after-init-hook (lambda () (load-theme 'modus-vivendi)))

(provide 'core-ui)
