;; make closing annoying popups easier: hit C-g to close
(use-package popwin
  :ensure t
  :config
  (popwin-mode t)

  ;; manage the buffers that are handled
  (setq popwin:special-display-config nil)
  (push '("\\*Flycheck.+\\*$" :regexp t :noselect t)
        popwin:special-display-config)
  (push '("*Compile-Log*" :noselect t)
        popwin:special-display-config)
  (push '("*Warnings*" :noselect t)
        popwin:special-display-config)
  (push '("*Help*" :noselect nil)
        popwin:special-display-config)
  (push '("*compilation*" :noselect t)
        popwin:special-display-config)
  (push '("*undo-tree*" :noselect nil)
        popwin:special-display-config))

;; ignore certain buffers when using winner undo
(use-package winner
  :ensure t
  :config
  (setq winner-boring-buffers '("*Completions*"
                                "*Compile-Log*"
                                "*Inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"))
  (winner-mode t))

;; set default font
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil
                      :family "DejaVu Sans Mono"
                      :weight 'normal))

;; switch to dark theme by default
(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

;; make matching parenthesis easier to spot
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; provide helpful hints
(use-package which-key
  :ensure t
  :delight
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.4))

;; configure mode-line
(use-package telephone-line
  :ensure t
  :config
  (setq telephone-line-lhs
       ;'((ryo . (telephone-line-ryo-modal-segment))
        '((accent . (telephone-line-vc-segment
                     telephone-line-process-segment))
          (nil . (telephone-line-minor-mode-segment
                  telephone-line-buffer-segment))))
  (setq telephone-line-rhs
        '((nil . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))))
         ;(ryo . (telephone-line-airline-position-segment))))

  ;TODO: set separators to curved - currently gives empty modeline in terminal mode
  (telephone-line-mode t))

(provide 'hge-gui-settings)
