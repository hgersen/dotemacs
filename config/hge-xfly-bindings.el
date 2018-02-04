;;  -*- coding: utf-8; lexical-binding: t; -*-

;; simulated key-presses to make some emacs prefix maps easier to access
(define-key xfly-command-key-map
  (kbd ";") (xfly-simulate-key-press "C-c"))

;; normal mode mappings
(xfly-define-keys
 xfly-command-key-map
 '(("SPC" . xfly-leader-key-map)
   ;; numbers
   ("1" . pop-global-mark)
   ("2" . xah-pop-local-mark-ring)
   ("3" . delete-other-windows)
   ("4" . split-window-below)
   ("5" . delete-char)
   ("6" . xah-select-block)
   ("7" . xah-select-line)
   ("8" . xah-extend-selection)
   ("9" . xah-select-text-in-quote)

   ;; left hand
   ("q" . xah-reformat-lines)
   ("w" . xah-shrink-whitespaces)
   ("f" . backward-kill-word)
   ("p" . kill-word)
   ("v" . set-mark-command)
   ("a" . execute-extended-command)
   ("r" . open-line)
   ("s" . xah-delete-backward-char-or-bracket-text)
   ("t" . xfly-command-mode-disable)
   ("g" . xah-delete-current-text-block)
   ("z" . xah-comment-dwim)
   ("x" . xah-cut-line-or-region)
   ("c" . xah-copy-line-or-region)
   ("d" . xah-paste-or-paste-previous)
   ("b" . xah-toggle-letter-case)

   ;; right hand
   ("j" . undo-tree-undo)
   ("J" . undo-tree-redo)
   ("l" . backward-word)
   ("u" . previous-line)
   ("y" . forward-word)
   ;; missing char in keymap
   ("k" . xah-beginning-of-line-or-block)
   ("n" . backward-char)
   ("e" . next-line)
   ("i" . forward-char)
   ("o" . xah-end-of-line-or-block)
   ("-" . xah-cycle-hyphen-underscore-space)
   ("m" . swiper)
   ("h" . xah-backward-left-bracket)
   ("," . xah-next-window-or-frame)
   ("." . xah-forward-right-bracket)
   ("/" . xah-goto-matching-bracket)))

;; leader key; equivalent to xah-fly-leader-key-map
(xfly-define-keys
 (define-prefix-command 'xfly-leader-key-map)
 '(("3" . delete-window)
   ("4" . split-window-right)
   ("5" . balance-windows)
   ("9" . ispell-word)
   ("a" . mark-whole-buffer)
   ("c" . xah-search-current-word)
   ("d" . xfly-d-key-map)
   ("e" . xfly-e-key-map)
   ("f" . xfly-f-key-map)
   ("h" . xfly-h-key-map)
   ("i" . xfly-i-key-map)
   ("o" . xfly-o-key-map)
   ("r" . xfly-r-key-map)
   ("s" . xfly-s-key-map)
   ("t" . xfly-t-key-map)
   ("w" . save-buffer)
   ("b" . switch-to-buffer)
   ("x" . xah-cut-all-or-region)
   ("." . repeat)))

;; delete and dump (=paste)
(xfly-define-keys
 (define-prefix-command 'xfly-d-key-map)
 '(("f" . delete-frame)))

;; evaluate and emacs; equivalent to xah-fly-w-keymap
(xfly-define-keys
 (define-prefix-command 'xfly-e-key-map)
 '(("a" . eval-buffer)
   ("d" . eval-defun)
   ("s" . eval-last-sexp)
   ("c" . eval-expression)
   ("e" . next-error)
   ("u" . previous-error)
   ("r" . eval-region)
   ("R" . restart-emacs)
   ("q" . save-buffers-kill-terminal)
   ("f" . xah-run-current-file)))

;; find and files; equivalent to xah-fly-c-keymap
(xfly-define-keys
 (define-prefix-command 'xfly-f-key-map)
 '(("e" . xah-open-in-external-app)
   ("f" . find-file)
   ("b" . bookmark-bmenu-list)
   ("i" . ibuffer)
   ("t" . xah-open-file-at-cursor)
   ("r" . counsel-recentf)
   ("m" . bookmark-set)
   ("n" . xah-new-empty-buffer)
   ("d" . xah-show-in-desktop)
   ("l" . xah-open-last-closed)
   ("y" . xah-list-recently-closed)
   ("o" . xah-open-file-fast)
   ("w" . write-file)))

;; help mode; equivalent to xah-fly-h-keymap
(xfly-define-keys
 (define-prefix-command 'xfly-h-key-map)
 '(("z" . Info-goto-emacs-command-node)
   ("a" . apropos-command)
   ("b" . describe-bindings)
   ("c" . describe-char)
   ("d" . apropos-documentation)
   ("e" . view-echo-area-messages)
   ("f" . describe-face)
   ("g" . info-lookup-symbol)
   ("i" . info)
   ("," . man)
   ("k" . describe-key)
   ("K" . Info-goto-emacs-key-command-node)
   ("l" . view-lossage)
   ("m" . xah-describe-major-mode)
   ("n" . describe-variable)
   ("o" . describe-language-environment)
   ("p" . finder-by-keyword)
   ("r" . apropos-variable)
   ("s" . describe-syntax)
   ("t" . elisp-index-search)
   ("v" . apropos-value)
   ("/" . describe-coding-system)))

;; indent and completions
(xfly-define-keys
 (define-prefix-command 'xfly-i-key-map)
 '(("i" . indent-for-tab-command)
   ("c" . complete-symbol)
   ("t" . indent-rigidly)
   ("r" . indent-region)
   ("s" . indent-sexp)
   ;; temp
   ("1" . abbrev-prefix-mark)
   ("2" . edit-abbrevs)
   ("3" . expand-abbrev)
   ("4" . expand-region-abbrevs)
   ("5" . unexpand-abbrev)
   ("6" . add-global-abbrev)
   ("7" . add-mode-abbrev)
   ("8" . inverse-add-global-abbrev)
   ("9" . inverse-add-mode-abbrev)
   ("0" . expand-jump-to-next-slot)
   ("=" . expand-jump-to-previous-slot)))

;; marks; equivalent to xah-fly-comma-keymap
(xfly-define-keys
 (define-prefix-command 'xfly-m-key-map)
 '(("m" . xref-pop-marker-stack)))

;; options and open; equivalent to xah-fly-n-keymap
(xfly-define-keys
 (define-prefix-command 'xfly-o-key-map)
 '(("SPC" . whitespace-mode)
   ("a" . abbrev-mode)
   ("," . toggle-frame-fullscreen)
   ("h" . global-hl-line-mode)
   ("l" . global-display-line-numbers-mode)
   ("v" . visual-line-mode)
   ("3" . calendar)
   ("4" . calc)
   ("t" . shell-command)
   ("r" . shell-command-on-region)
   ("d" . toggle-debug-on-error)
   ("c" . toggle-case-fold-search)
   ("e" . eshell)
   ("f" . make-frame-command)
   ("s" . flyspell-buffer)
   ("m" . menu-bar-open)
   ("w" . toggle-word-wrap)
   ("p" . shell)
   ("r" . read-only-mode)
   ("/" . abort-recursive-edit)))

;; rectangle; equivalent to xah-fly-r-keymap
(xfly-define-keys
 (define-prefix-command 'xfly-r-key-map)
 '(("SPC" . rectangle-mark-mode)
   ("a" . apply-macro-to-region-lines)
   ("s" . kmacro-start-macro)
   ("3" . number-to-register)
   ("4" . increment-register)
   ("b" . xah-upcase-sentence)
   ("i" . replace-rectangle)
   ("d" . delete-rectangle)
   ("e" . call-last-kbd-macro)
   ("k" . kill-rectangle)
   ("w" . clear-rectangle)
   ("l" . xah-space-to-newline)
   ("n" . rectangle-number-lines)
   ("o" . open-rectangle)
   ("f" . kmacro-end-macro)
   ("r" . yank-rectangle)
   ("q" . xah-quote-lines)
   ("x" . delete-whitespace-rectangle)))

;; surround objects; based on xah-fly-e-map
(xfly-define-keys
 (define-prefix-command 'xfly-s-key-map)
 '(("RET" . insert-char)
   ("e" . xah-insert-emacs-quote)
   ("w" . xah-insert-brace)
   ("s" . xah-insert-paren)
   ("t" . xah-insert-square-bracket)))

;; to; equivalent to xah-fly-t-keymap
(xfly-define-keys
 (define-prefix-command 'xfly-t-key-map)
 '(("SPC" . xah-clean-whitespace)
   ("TAB" . move-to-column)
   ("1" . xah-clear-register-1)
   ("2" . xah-append-to-register-1)
   ("3" . xah-copy-to-register-1)
   ("4" . xah-paste-from-register-1)
   ("d" . xref-find-definitions)
   ("l" . goto-line)
   ("w" . xah-next-window-or-frame)))

(provide 'hge-xfly-bindings)
