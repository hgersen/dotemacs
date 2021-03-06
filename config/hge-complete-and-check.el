;;;  -*- coding: utf-8; lexical-binding: t; -*-

;; spell checking using flyspell
(use-package flyspell
  :straight t
  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-correct-previous-word-generic))
  :hook ((text-mode . flyspell-mode)
         (org-mode . flyspell-mode))
  :config
  (setq ispell-program-name "hunspell"
        ispell-really-hunspell t
        ispell-dictionary "en_GB"))

(use-package flyspell-correct-ivy
  :straight t
  :delight
  :after flyspell)

;; syntax checking
(use-package flycheck
  :straight t
  :commands flycheck-mode
  :config
  ;; reduce visual noise by forcing to initialise packages for all lisp files
  (setq-default flycheck-emacs-lisp-initialize-packages t
                flycheck-emacs-lisp-load-path 'inherit
                flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (global-flycheck-mode t))

;; completion
(use-package company
  :straight t
  :init
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t)
  :config
  ;; work-around as :bind fails to work
  (bind-keys
     :map company-active-map
        ("C-e" . company-select-next)
        ("C-u" . company-select-previous)
        ("C-i" . company-complete-common-or-cycle)
        ("C-o" . company-complete-selection)
        ;; prevent auto-complete on enter; both appear needed
        ("<return>" . nil)
        ("RET" . nil))
  (global-company-mode 1))

(provide 'hge-complete-and-check)
