;; set up repositories
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; do not load outdated elc files
(setq load-prefer-newer t)

;; initialize the package system
(package-initialize)

;; only load use-package when compiling to reduce load-time
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "libs/use-package" user-emacs-directory))
  (require 'use-package))

;; warn when loading a package takes too long
(setq use-package-verbose t)

;; fix packages not installing because list is out of date
(defvar my/refreshed-package-list nil
  "This wil be t if the package list has been refreshed.")

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

;; set directory to load additional configurations from
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; load settings from configuration file
;; (require 'hge-editor-core)

;; silence the default GNU startup message for myself
(eval '(setq inhibit-startup-echo-area-message "hge"))

;; display time it took to load the configuration
(message "[startup] loading %s ... took %s" load-file-name (emacs-init-time))
