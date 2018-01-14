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

;; build in package misc for word-movement
(use-package misc
  :ensure nil)

;; roll my own modal mode
(use-package ryo-modal
  :ensure nil
  ;; demand is needed to prevent errors; relating to :ryo-keyword
  :demand t
  :delight
  :load-path "/home/hge/elisp/ryo-modal"
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

(provide 'hge-editor-core)
