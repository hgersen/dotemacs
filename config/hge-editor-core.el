;; wrappers for entering and exiting
(defun my/enable-ryo-modal-mode ()
  "Explicitly enables ryo-modal mode"
  (interactive)
  (ryo-modal-mode 1))

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

;; useful extensions for emacs
(use-package crux
  :ensure nil
  :load-path "libs/crux")

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

(use-package ryo-modal-core-keys
  :ensure nil
  :ryo
  ;; digit mappings
  (:norepeat t)
  ("1" "M-1" :name "1")
  ("2" "M-2" :name "2")
  ("3" "M-3" :name "3")
  ("4" "M-4" :name "4")
  ("5" "M-5" :name "5")
  ("6" "M-6" :name "6")
  ("7" "M-7" :name "7")
  ("8" "M-8" :name "8")
  ("9" "M-9" :name "9")

  ;; modal movement
  ("k" "C-b" :name "prev char")
  ("n" "C-n" :name "next line")
  ("e" "C-p" :name "prev line")
  ("i" "C-f" :name "next char")

  ;; word movements
  ("f" forward-to-word :name "forward to word")
  ("b" "M-b" :name "backward to word")

  ;; modal insert editing maps
  ("o" my/disable-ryo-modal-mode :name "insert")
  ("O" crux-move-beginning-of-line :name "insert at start of line" :exit t)
  ("a" forward-char :name "append" :exit t)
  ("A" move-end-of-line :name "append end of line" :exit t)

  ;; modal editing
  ("p" "C-y" :name "paste")
  ("x" delete-char :name "delete char")

  ;; undo/redo
  ("u" "C-/" :name "undo")
  ("U" "M-/" :name "repeat"))

(provide 'hge-editor-core)
