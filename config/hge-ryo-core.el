;; wrappers for entering and exiting
(defun my/enable-ryo-modal-mode ()
  "Explicitly enables ryo-modal mode"
  (interactive)
  (ryo-modal-mode 1))

(defun my/disable-ryo-modal-mode ()
  "Explicitly disables ryo-modal mode"
  (interactive)
  (ryo-modal-mode -1))

(defun my/last-char-in-line (arg)
  "Jump to last non-white character in line"
  (interactive "p")
  (end-of-line arg)
  (unless (bolp) (backward-char)))

;; enable the use of spacemacs like transient states
(use-package hydra
  :ensure t
  :init
  (setq hydra-if-helpful t))

;; provide helpful hints
(use-package which-key
  :ensure t
  :delight
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.4))

;; roll my own modal mode
(use-package ryo-modal
  :ensure nil
  ;; demand is needed to prevent errors; not sure why
  :demand t
  :delight
  :load-path "libs/ryo-modal"
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

  ;; cursor for insert mode
  (setq-default cursor-type 'bar)
  (set-cursor-color "chartreuse3")

  ;; cursor for ryo modal mode
  (setq ryo-modal-cursor-color "DarkGoldenrod2"
        ryo-modal-cursor-type 'box))

(provide 'hge-ryo-core)
