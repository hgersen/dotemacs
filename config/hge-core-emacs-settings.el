;; keep directories tidy
(use-package tidy-directories
  :ensure nil
  :init
  ;; hide the cache
  (defconst user-cache-directory
    (file-name-as-directory (concat user-emacs-directory ".cache")))
  (when (not (file-exists-p user-cache-directory))
    (make-directory user-cache-directory t))

  ;; put all backup files in single directory
  (defconst user-backup-directory
    (file-name-as-directory (concat user-emacs-directory ".backup")))
  (when (not (file-exists-p user-backup-directory))
    (make-directory user-backup-directory t))
  (setq backup-directory-alist `((".*" . ,user-backup-directory)))

  ;; keep temporary files out of the way
  (defconst user-temp-directory
    (file-name-as-directory (concat user-emacs-directory ".temp")))
  (when (not (file-exists-p user-temp-directory))
    (make-directory user-temp-directory t))
  (setq auto-save-file-name-transforms `((".*" ,user-temp-directory t)))
  (provide 'tidy-directories))

;; backup settings
(use-package backup-settings
  :ensure nil
  :after tidy-directories
  :init
  (setq vc-make-backup-files t ; backup versioned files
        make-backup-files t ; backup a file the first time it is saved
        backup-by-copying t ; don't clobber symlinks
        version-control t ; version numbers for backup files
        delete-old-versions t ; delete excess backups silently
        kept-old-versions 5 ; number of original versions to keep (default 2)
        kept-new-versions 5 ; number of recent versions to keep (default 2)
        auto-save-default t ; auto-save every buffer that visits a file
        )
  (provide 'backup-settings))

;; global settings
(use-package global-settings
  :ensure nil
  :init
  ;; shorten yes/no answers
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; confirm exit
  (setq confirm-kill-emacs 'y-or-n-p)
  ;; improve default behavior on tabs
  (setq-default indent-tabs-mode nil
                default-tab-width 4
                c-default-style "lisp"
                c-basic-offset 4
                fill-column 90
                sentence-end-double-space nil)
  ;; make text auto-fill by default
  (add-hook 'text-mode-hook 'auto-fill-mode)
  ;; keep track of locations in file
  (setq save-place-file (concat user-cache-directory "saveplace"))
  (save-place-mode t)
  (provide 'global-settings))

;; visual settings
(use-package visual-settings
  :ensure nil
  :init
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (setq inhibit-startup-message t)
  ;; visual aids for code-editing
  (setq show-paren-delay 0)
  (show-paren-mode t)
  ;; tune the space space in the fringe
  (fringe-mode '(8 . 8))
  (provide 'visual-settings))

;; make core editor config settings available
(provide 'hge-core-emacs-settings)
