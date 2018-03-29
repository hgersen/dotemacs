;;;  -*- coding: utf-8; lexical-binding: t; -*-

(use-package rust-mode
  :straight t
  :mode "\\.rs\\'"
  :hook
  ((rust-mode . company-mode)
   (rust-mode . flycheck-mode))
  :config
  (setq rust-format-on-save t))

;; run cargo commands from emacs
(use-package cargo
  :straight t
  :after rust-mode
  :mode "\\.rs\\'"
  :hook
  (rust-mode . cargo-minor-mode))

;; improves rust/cargo support in flycheck
(use-package flycheck-rust
  :straight t
  :after rust-mode cargo
  :mode "\\.rs\\'"
  :hook
  (flycheck-mode . flycheck-rust-setup))

;; code completion and source navigation for Rust
(use-package racer
  :straight t
  :after rust-mode company
  :mode "\\.rs\\'"
  :init
  (setq racer-cmd "~/.cargo/bin/racer"
        racer-rust-src-path "~/coding/rust/src")
  :hook
  ((rust-mode . racer-mode)
   (racer-mode . eldoc-mode)
   (racer-mode . company-mode)))

;; support toml for cargo
(use-package toml-mode
  :straight t
  :mode "/\\(Cargo.lock\\|\\.cargo/config\\)\\'")

(provide 'hge-programming)
