;;;  -*- coding: utf-8; lexical-binding: t; -*-

;; used to provide spacemacs like transient states
(use-package hydra
  :straight t
  :init
  (setq hydra-if-helpful t))

;; provide helpful hints
(use-package which-key
  :straight t
  :delight
  :config
  ;; prevent showing ryo- in front of commands in popup
  (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist)
  (which-key-mode)
  (setq which-key-idle-delay 0.4))

;; roll my own modal mode
(use-package ryo-modal
  :straight t
  ;; use demand to prevent errors relating to :ryo keyword
  :demand t
  :delight
  ;; the *version takes precedence over other minor mode keybindings
  :bind* ("<escape>" . ryo-command-mode-enable)
  ;; repeat needs to be bound to keymap
  :hook ((after-init . ryo-command-mode-enable)
         (text-mode . ryo-command-mode-enable)
         (prog-mode . ryo-command-mode-enable))
  :init
  ;; Emacs has point between characters; stick with bar to reflect this
  ;; cursor for insert mode
  (setq-default cursor-type 'bar)
  (set-cursor-color "chartreuse3")

  ;; cursor for ryo modal mode
  (setq ryo-modal-cursor-color "DarkGoldenrod2"
        ryo-modal-cursor-type 'bar))

;; offer functions to help in profiling startup
(use-package esup
  :commands esup
  :straight t)

;; provide graphical tools to undo
(use-package undo-tree
  :straight t
  :commands (undo-tree-undo
             undo-tree-redo)
  :delight
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
  :commands (recentf-mode
             recentf-add-file
             recentf-apply-filename-handlers)
  :config
  (setq recentf-max-saved-items 1000
        recentf-auto-cleanup 300
        recentf-save-file (expand-file-name "recentf" user-cache-directory)
        recentf-exclude '("/tmp/"
                          "/Maildir/"))
  (recentf-mode 1))

;; make restarting emacs a bit easier
(use-package restart-emacs
  :commands restart-emacs
  :straight t)

;; smex is used to get frequent commands at the top
(use-package smex
  :straight t
  :commands smex
  :config
  (setq smex-save-file (concat user-cache-directory "smex-items"))
  (smex-initialize))

;; replaces isearch
(use-package swiper
  :straight t
  :commands swiper
  :after ivy)

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
  :after ivy
  :demand t
  :delight
  :config
  (counsel-mode t))

;; use projectile to interact with projects
(use-package projectile
  :straight t
  :commands projectile-global-mode
  :delight
  :config
  (setq projectile-cache-file (expand-file-name "projectile.cache" user-cache-directory)
        projectile-known-projects-file (concat user-cache-directory "projectile-bookmarks.eld")
        projectile-completion-system 'ivy)
  (projectile-global-mode))

(use-package counsel-projectile
  :straight t
  :commands counsel-projectile-mode
  :after (counsel projectile)
  :config
  (counsel-projectile-mode))

;; remove white-space on lines that have been edited
(use-package ws-butler
  :straight t
  :delight
  :commands ws-butler
  :config
  (ws-butler-global-mode 1))

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
