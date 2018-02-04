;;;  -*- coding: utf-8; lexical-binding: t; -*-

;; used to provide spacemacs like transient states
(use-package hydra
  :disabled t
  :straight t
  :init
  (setq hydra-if-helpful t))

;; provide helpful hints
(use-package which-key
  :straight t
  :delight
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.4))

;; roll my own modal mode
(use-package xfly
  :ensure nil
  :delight
  ;; the *version takes precedence over other minor mode keybindings
  :bind* ("<escape>" . xfly-command-mode-enable)
  ;; repeat needs to be bound to keymap
  :hook ((after-init . xfly-command-mode-enable)
         (text-mode . xfly-command-mode-enable)
         (prog-mode . xfly-command-mode-enable))
  :init
  ;; Emacs has point between characters; stick with bar to reflect this
  ;; cursor for insert mode
  (setq-default cursor-type 'bar)
  (set-cursor-color "chartreuse3")

  ;; cursor for ryo modal mode
  (setq xfly-modal-cursor-color "DarkGoldenrod2"
        xfly-modal-cursor-type 'bar))

;; provide graphical tools to undo
(use-package undo-tree
  :straight t
  :delight
  ;; :ryo
  ;; (:norepeat t)
  ;; ("j" undo-tree-undo)
  ;; ("J" undo-tree-redo)
  :config
  (defconst user-undo-directory
    (file-name-as-directory (concat user-emacs-directory ".undo")))
  (when (not (file-exists-p user-undo-directory))
    (make-directory user-undo-directory t))
  (setq undo-tree-history-directory-alist `(("." . ,user-undo-directory))
        undo-tree-auto-save-history t
        undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))

;; keep track of previously opened files
(use-package recentf
  :straight t
  :config
  (setq recentf-max-saved-items 1000
        recentf-auto-cleanup 300
        recentf-save-file (expand-file-name "recentf" user-cache-directory)
        recentf-exclude '("/tmp/"
                          "/Maildir/"))
  (recentf-mode 1))

;; make restarting emacs a bit easier
(use-package restart-emacs
  :straight t)

;; smex is used to get frequent commands at the top
(use-package smex
  :straight t
  :config
  (setq smex-save-file (concat user-cache-directory "smex-items"))
  (smex-initialize))

;; replaces isearch
(use-package swiper
  :straight t)

;; generic completion front-end
(use-package ivy
  :straight t
  :delight
  :config
  (setq ivy-use-virtual-buffers t
        ivy-wrap t
        ivy-display-style 'fancy
        ivy-count-format "(%d/%d ")
  (ivy-mode t))

;; provide completion functions that use ivy
(use-package counsel
  :straight t
  :delight
  :config
  (counsel-mode t))

;; use projectile to interact with projects
(use-package projectile
  :straight t
  :delight
  :config
  (setq projectile-cache-file (expand-file-name "projectile.cache" user-cache-directory)
        projectile-known-projects-file (concat user-cache-directory "projectile-bookmarks.eld")
        projectile-completion-system 'ivy)
  (projectile-global-mode))

(use-package counsel-projectile
  :straight t
  :config
  (counsel-projectile-mode))

;; folding
(use-package vimish-fold
  :disabled t
  :straight t
  :config
  (defhydra hydra-vimish-fold ()
    ("f" vimish-fold "fold region")
    ("d" vimish-fold-delete "delete")
    ("D" vimish-fold-delete-all "delete all")
    ("n" vimish-fold-next-fold "next fold")
    ("e" vimish-fold-previous-fold "previous fold")
    ("o" vimish-fold-unfold "open")
    ("c" vimish-fold-refold "close")
    ("u" vimish-fold-unfold-all "unfold all")
    ("r" vimish-fold-refold-all "refold all"))
  (vimish-fold-global-mode 1))

(provide 'hge-core-packages)
