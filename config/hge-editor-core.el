;; wrappers for entering and exiting
;;;###autoload
(defun my/enable-ryo-modal-mode ()
  "Explicitly enables ryo-modal mode"
  (interactive)
  (ryo-modal-mode 1))

;;;###autoload
(defun my/disable-ryo-modal-mode ()
  "Explicitly disables ryo-modal mode"
  (interactive)
  (ryo-modal-mode -1))

;; enable the use of spacemacs like transient states
(use-package hydra
  :straight t
  :init
  (setq hydra-if-helpful t))

;; provide helpful hints
(use-package which-key
  :straight t
  :delight
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.4))

;; build in package misc for word-movement
(use-package misc
  :ensure nil)

;; roll my own modal mode
(use-package ryo-modal
  :straight t
  ;; demand is needed to prevent errors; relating to :ryo-keyword
  :demand t
  :delight
  ;; the *version takes precedence over other minor mode keybindings
  :bind* ("<escape>" . my/enable-ryo-modal-mode)
  ;; repeat needs to be bound to keymap
  :bind (:map ryo-modal-mode-map
              ("." . ryo-modal-repeat))
  :hook ((after-init . my/enable-ryo-modal-mode)
         (text-mode . my/enable-ryo-modal-mode)
         (prog-mode . my/enable-ryo-modal-mode))
  :init
  ;; remove ryo key word from which-key
  (push '((nil . "ryo:") . (nil . "")) which-key-replacement-alist)

  ;; Emacs has point between characters; stick with bar to reflect this
  ;; cursor for insert mode
  (setq-default cursor-type 'bar)
  (set-cursor-color "chartreuse3")

  ;; cursor for ryo modal mode
  (setq ryo-modal-cursor-color "DarkGoldenrod2"
        ryo-modal-cursor-type 'bar))

;; provide graphical tools to undo
(use-package undo-tree
  :straight t
  :ryo
  (:norepeat t)
  ("u" undo-tree-undo)
  ("U" undo-tree-redo)
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

(provide 'hge-editor-core)
