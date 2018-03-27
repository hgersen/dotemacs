;;;  -*- coding: utf-8; lexical-binding: t; -*-

;; make closing annoying popups easier: hit C-g to close
(use-package popwin
  :straight t
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
  :straight t
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

;; switch to dark theme by default
(use-package monokai-theme
  :straight t
  :config
  (load-theme 'monokai t))

;; make matching parenthesis easier to spot
(use-package rainbow-delimiters
  :straight t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; configure mode-line
(use-package telephone-line
  :straight t
  :config
  (setq telephone-line-evil-use-short-tag t)
  (telephone-line-defsegment* telephone-line-xfly-modal-segment ()
     (let ((tag (if xfly-modal-mode
                    "COMMAND" "EMACS")))
       (if telephone-line-evil-use-short-tag
           (seq-take tag 1)
         tag)))

  (defun telephone-line-xfly-modal-face (active)
    "Return an appropriate face depending whether xfly-mode is activated."
    (cond ((not active) 'mode-line-inactive)
          ((not (boundp 'xfly-modal-mode)) 'mode-line)
          ((not xfly-modal-mode) 'telephone-line-evil-insert)
          (t 'telephone-line-evil-normal)))

  (add-to-list 'telephone-line-faces '(xfly . telephone-line-xfly-modal-face))
  (setq telephone-line-lhs
        '((xfly . (telephone-line-xfly-modal-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-process-segment))
          (nil . (telephone-line-minor-mode-segment
                  telephone-line-buffer-segment))))
  (setq telephone-line-rhs
        '((nil . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (xfly . (telephone-line-airline-position-segment))))
  (telephone-line-mode t))

(provide 'hge-gui-settings)
