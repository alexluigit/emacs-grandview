;; General
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; make ESC quit prompts

;; Hydra (on the fly keybind)
(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("n" text-scale-increase "in")
  ("e" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; Leader keys
(use-package general
  :config
  (general-create-definer alex/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (defun alex/edit-configuration ()
  "Open the init file."
    (interactive)
    (find-file "~/Dev/alex.files/config/emacs/init.el"))
  (alex/leader-keys
    "/"  '(. evilnc-comment-or-uncomment-lines)
    "f"  '(:ignore t :which-key "find files")
    "f." '(alex/edit-configuration :which-key "emacs config")
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "ts" '(hydra-text-scale/body :which-key "scale text")
    "p"  '(projectile-command-map :which-key "projectile")
    "g"  '(magit-status :which-key "magit")
    "q"  '(kill-this-buffer :which-key "kill-buffer")
    "w"  '(save-buffer :which-key "save buffer")))

;; Evil Mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-f") 'swiper)
  (define-key evil-normal-state-map (kbd "C-n") 'evil-search-next)
  (define-key evil-normal-state-map (kbd "C-e") 'evil-search-previous)
  (define-key evil-normal-state-map (kbd "k")   'evil-forward-word-end)
  (define-key evil-normal-state-map (kbd "K")   'evil-forward-WORD-end)
  (define-key evil-normal-state-map (kbd "E")   'evil-lookup)
  ;; use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "n"   'evil-next-visual-line)
  (evil-global-set-key 'motion "e"   'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))
  ;; enable evil mode in other modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Ivy
(use-package ivy
  :diminish
  :bind (:map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-'" . ivy-alt-done)
         ("C-n" . ivy-next-line)
         ("C-e" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-e" . ivy-previous-line)
         ("C-'" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-e" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; Counsel
(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))
