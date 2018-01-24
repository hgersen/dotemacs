;; macros to reduce code duplication
(defmacro defhymop (name body operation back-to &optional docstring &rest heads)
  "wrapper for defhydra that performs region operation on selected text-objects"

  (declare (indent defun))
  ;; deal with no supplied doctring
  (setq hym-heads (copy-tree heads))
  (unless (stringp docstring)
    (setq hym-heads (cons docstring hym-heads))
    (setq docstring "hymop"))
  `(defhydra ,name ,body ,docstring
     ;; add interactive functions that operate on marked regions
     ,(mapcar (lambda (head)
                `(,(car head) (lambda (count)
                                (interactive "p")
                                (,@(cdr head) count)
                                (call-interactively (quote ,operation))
                                ,(when back-to
                                   `((setq current-prefix-arg nil)
                                     (,back-to))))
                  :exit t)) hym-heads)))

;; define macro to define delete operation
(defmacro defvim-delop (name &rest forms)
  (declare (indent 1))
  `(defun ,name (count)
     (interactive "p")
     ,@forms
     (call-interactively 'kill-region)))

;; define macro for copy(yank) operations; returns to normal mode
(defmacro defvim-yankop-ret (name &rest forms)
  (declare (indent 1))
  `(defun ,name (count)
     (interactive "p")
     ,@forms
     (call-interactively 'copy-region-as-kill)
     (setq current-prefix-arg nil)
     (hydra-normal-mode/body)))

;; define macro for delete operation that returns to normal mode
(defmacro defvim-delop-ret (name &rest forms)
  (declare (indent 1))
  `(defun ,name (count)
     (interactive "p")
     ,@forms
     (call-interactively 'kill-region)
     (setq current-prefix-arg nil)
     (hydra-normal-mode/body)))

;; build in package misc for word-movement
(use-package misc
  :ensure nil)

;; used to provide both modal modes and spacemacs like transient states
(use-package hydra
  :straight t
  :init
  (setq hydra-if-helpful t))

;; provide keybindings for vim-like "normal" mode
(use-package my/hydra-normal-mode
  :ensure nil
  ;; the *version takes precedence over other minor mode keybindings
  :bind* ("<escape>" . hydra-normal-mode/body)
  :init
  (provide 'my/hydra-normal-mode)
  :config

  (defhydra hydra-normal-mode (:foreign-keys warn)
    ;; motions
    ("k" backward-char "prev char")
    ("n" next-line "next line")
    ("e" previous-line "prev line")
    ("i" forward-char "next char")
    ("w" forward-to-word "forward to word")
    ("b" backward-word "backwards to word")
    ("N" forward-paragraph "next paragraph")
    ("E" backward-paragraph "prev paragraph")

    ;; operators
    ("c" hydra-change-operations/body "change" :exit t)
    ("d" hydra-delete-operations/body "delete" :exit t)
    ("y" hydra-yank-operations/body "copy" :exit t)

    ;; actions
    ("z" hydra-vimish-fold/body "folding" :exit t)

    ;; exit
    ("<escape>" nil "quit to insert"))

  ;; define command that delete text-objects
  (defvim-delop h-del-char-backward (my/mark-backward-char count))
  (defvim-delop h-del-char-forward (my/mark-forward-char count))
  (defvim-delop h-del-word-backward (my/mark-backward-word count))
  (defvim-delop h-del-word-forward (my/mark-forward-word count))
  (defvim-delop h-del-line-backward (my/mark-backward-line count))
  (defvim-delop h-del-line-forward (my/mark-forward-line count))
  (defvim-delop h-del-paragraph-backward (my/mark-backward-paragraph count))
  (defvim-delop h-del-paragraph-forward (my/mark-forward-paragraph count))

  ;; copy operations; drop to insert mode
  (defhydra hydra-change-operations (:foreign-keys warn :exit t)
    "copy"
    ("k" h-del-char-backward :exit t)
    ("n" h-del-line-forward :exit t)
    ("e" h-del-line-backward :exit t)
    ("i" h-del-char-forward :exit t)
    ("w" h-del-word-forward :exit t)
    ("b" h-del-word-backward :exit t)
    ("N" h-del-paragraph-forward :exit t)
    ("E" h-del-paragraph-backward :exit t))

  ;; define commands that delete text-objects, but return to normal mode
  (defvim-delop-ret h-del-char-backward-ret (my/mark-backward-char count))
  (defvim-delop-ret h-del-char-forward-ret (my/mark-forward-char count))
  (defvim-delop-ret h-del-word-backward-ret (my/mark-backward-word count))
  (defvim-delop-ret h-del-word-forward-ret (my/mark-forward-word count))
  (defvim-delop-ret h-del-line-backward-ret (my/mark-backward-line count))
  (defvim-delop-ret h-del-line-forward-ret (my/mark-forward-line count))
  (defvim-delop-ret h-del-paragraph-backward-ret (my/mark-backward-paragraph count))
  (defvim-delop-ret h-del-paragraph-forward-ret (my/mark-forward-paragraph count))

  ;; delete operations; stay in normal mode
  (defhydra hydra-delete-operations (:foreign-keys warn)
    "delete"
    ("k" h-del-char-backward-ret :exit t)
    ("n" h-del-line-forward-ret :exit t)
    ("e" h-del-line-backward-ret :exit t)
    ("i" h-del-char-forward-ret :exit t)
    ("w" h-del-word-forward-ret :exit t)
    ("b" h-del-word-backward-ret :exit t)
    ("N" h-del-paragraph-forward-ret :exit t)
    ("E" h-del-paragraph-backward-ret :exit t))

  ;; define commands that yank text-objects, but return to normal mode
  (defvim-yankop-ret h-yank-char-backward-ret (my/mark-backward-char count))
  (defvim-yankop-ret h-yank-char-forward-ret (my/mark-forward-char count))
  (defvim-yankop-ret h-yank-word-backward-ret (my/mark-backward-word count))
  (defvim-yankop-ret h-yank-word-forward-ret (my/mark-forward-word count))
  (defvim-yankop-ret h-yank-line-backward-ret (my/mark-backward-line count))
  (defvim-yankop-ret h-yank-line-forward-ret (my/mark-forward-line count))
  (defvim-yankop-ret h-yank-paragraph-backward-ret (my/mark-backward-paragraph count))
  (defvim-yankop-ret h-yank-paragraph-forward-ret (my/mark-forward-paragraph count))
  (defvim-yankop-ret h-yank-whole-line-ret (kill-whole-line count))

  ;; delete operations; stay in normal mode
  (defhydra hydra-yank-operations (:foreign-keys warn)
    "yank"
    ("k" h-yank-char-backward-ret :exit t)
    ("n" h-yank-line-forward-ret :exit t)
    ("e" h-yank-line-backward-ret :exit t)
    ("i" h-yank-char-forward-ret :exit t)
    ("w" h-yank-word-forward-ret :exit t)
    ("b" h-yank-word-backward-ret :exit t)
    ("N" h-yank-paragraph-forward-ret :exit t)
    ("E" h-yank-paragraph-backward-ret :exit t)))

;; provide helpful hints
(use-package which-key
  :straight t
  :delight
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.4))

;; roll my own modal mode
(use-package ryo-modal
  :disabled t
  :straight t
  ;; demand is needed to prevent errors; relating to :ryo-keyword
  :demand t
  :delight
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

  ;; Emacs has point between characters; stick with bar to reflect this
  ;; cursor for insert mode
  (setq-default cursor-type 'bar)
  (set-cursor-color "chartreuse3")

  ;; cursor for ryo modal mode
  (setq ryo-modal-cursor-color "DarkGoldenrod2"
        ryo-modal-cursor-type 'bar))

;; provide graphical tools to undo
(use-package undo-tree
  :straight t
                                        ;  :ryo
                                        ;  (:norepeat t)
                                        ;  ("u" undo-tree-undo)
                                        ;  ("U" undo-tree-redo)
  :config
  (defconst user-undo-directory
    (file-name-as-directory (concat user-emacs-directory ".undo")))
  (when (not (file-exists-p user-undo-directory))
    (make-directory user-undo-directory t))
  (setq undo-tree-history-directory-alist `(("." . ,user-undo-directory))
        undo-tree-auto-save-history t
        undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))

;; keep track of previously opened files
(use-package recentf
  :straight t
  :config
  (setq recentf-max-saved-items 1000
        recentf-auto-cleanup 300
        recentf-save-file (expand-file-name "recentf" user-cache-directory)
        recentf-exclude '("/tmp/"
                          "/Maildir/"))
  (recentf-mode 1))

;; folding
(use-package vimish-fold
  :straight t
  :config
  (defhydra hydra-vimish-fold ()
    ("f" vimish-fold "fold region")
    ("d" vimish-fold-delete "delete")
    ("D" vimish-fold-delete-all "delete all")
    ("n" vimish-fold-next-fold "next fold")
    ("e" vimish-fold-previous-fold "previous fold")
    ("o" vimish-fold-unfold "open")
    ("c" vimish-fold-refold "close")
    ("u" vimish-fold-unfold-all "unfold all")
    ("r" vimish-fold-refold-all "refold all"))
  (vimish-fold-global-mode 1))

(provide 'hge-editor-core)
