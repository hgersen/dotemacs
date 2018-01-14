;; Increase Garbage Collection to ease startup
(setq gc-cons-threshold (* 500 1024 1024))

;; set up repositories
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; do not load outdated elc files
(setq load-prefer-newer t)

;; make common libs extensions available
(require 'cl-lib)

;; initialize the package system
(package-initialize)

;; only load use-package when compiling to reduce load-time
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "libs/use-package" user-emacs-directory))
  (require 'use-package))

;; warn when loading a package takes too long
(setq use-package-verbose t
      use-package-minimum-reported-time 0.01)

;; fix packages not installing because list is out of date
(defvar my/refreshed-package-list nil
  "This wil be t if the package list has been refreshed.")

;;;###autoload
(defun my/ensure-refreshed ()
  "Ensure that the package list gets refreshed this startup"
  (unless my/refreshed-package-list
    (package-refresh-contents)
    (setq my/refreshed-package-list t)))

(advice-add 'package-install
            :before
            (lambda (&rest args)
              (my/ensure-refreshed)))

;; keep the mode-line clean; provides the :delight keyword in use-package
(use-package delight
  :ensure t)

;; some package claim they benefit from compilation, so ensure it happens
(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-save-mode)
  (auto-compile-on-load-mode))

;; prevent init-file polution by custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; keep directories tidy
(use-package tidy-directories
  :ensure nil
  :init
  (provide 'tidy-directories)
  :config
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
  (setq auto-save-file-name-transforms `((".*" ,user-temp-directory t))))

;; backup settings
(use-package backup-settings
  :ensure nil
  :after tidy-directories
  :init
  (provide 'backup-settings)
  :config
  (setq vc-make-backup-files t ; backup versioned files
        make-backup-files t ; backup a file the first time it is saved
        backup-by-copying t ; don't clobber symlinks
        version-control t ; version numbers for backup files
        delete-old-versions t ; delete excess backups silently
        kept-old-versions 5 ; number of original versions to keep (default 2)
        kept-new-versions 5 ; number of recent versions to keep (default 2)
        auto-save-default t ; auto-save every buffer that visits a file
        ))

;; global settings
(use-package global-settings
  :ensure nil
  :after tidy-directories
  ; make text auto-fill by default
  :hook (text-mode . auto-fill-mode)
  :init
  (provide 'global-settings)
  :config
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
  ;; keep track of locations in file
  (setq save-place-file (concat user-cache-directory "saveplace"))
  (save-place-mode t)
  ;; set coding system to utf-8
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))

;; visual settings
(use-package visual-settings
  :ensure nil
  :init
  (provide 'visual-settings)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  (tool-bar-mode -1)
  ;; silence GNU startup messages
  (setq inhibit-startup-message t)

  :config
  ;; visual aids for code-editing
  (setq show-paren-delay 0)
  (show-paren-mode t)
  ;; tune the space space in the fringe
  (fringe-mode '(8 . 8)))

;; silence default GNU startup for myself
(eval '(setq inhibit-startup-echo-area-message "hge"))

;; set default font
(use-package font-settings
  :ensure nil
  :init
  (provide 'font-settings)
  :config
  (when (member "DejaVu Sans Mono" (font-family-list))
    (set-face-attribute 'default nil
                        :family "DejaVu Sans Mono"
                        :weight 'normal)))

;; set directory to load additional configurations from
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; load additional settings from configuration files
(require 'hge-editor-core)
(require 'hge-gui-settings)

;; reduce garbage collection to happen often; threshold at 5MB
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))

;; display time it took to load the configuration
(message "[startup] loading %s ... took %s" load-file-name (emacs-init-time))
