;; ryo modal digit mappings
(ryo-modal-keys
 (:norepeat t)
 ("1" "M-1" :name "1")
 ("2" "M-2" :name "2")
 ("3" "M-3" :name "3")
 ("4" "M-4" :name "4")
 ("5" "M-5" :name "5")
 ("6" "M-6" :name "6")
 ("7" "M-7" :name "7")
 ("8" "M-8" :name "8")
 ("9" "M-9" :name "9"))

;; modal movement
(ryo-modal-keys
 (:norepeat t)
 ("k" "C-b" :name "prev char")
 ("n" "C-n" :name "next line")
 ("e" "C-p" :name "prev line")
 ("i" "C-f" :name "next char"))

;; word movements
(ryo-modal-keys
 (:norepeat t)
 ("f" forward-to-word :name "forward to word")
 ("b" "M-b" :name "backward to word")
 ("{" "M-{" :name "next paragraph")
 ("}" "M-}" :name "prev paragraph"))

;; modal insert editing maps
(ryo-modal-keys
 ("h" my/disable-ryo-modal-mode :name "insert here")
 ("H" my/move-beginning-of-line :name "insert at start of line" :exit t)
 ("o" my/open-line-below :name "open line below" :exit t)
 ("O" my/open-line-above :name "open line above" :exit t)
 ("a" forward-char :name "append" :exit t)
 ("A" move-end-of-line :name "append end of line" :exit t))

;; modal editing
(ryo-modal-keys
 (:norepeat t)
 ("p" "C-y" :name "paste")
 ("x" delete-char :name "delete char"))

;; window navigation
(ryo-modal-keys
 (:norepeat t)
 ("w o" switch-to-buffer-other-window :name "switch other window")
 ("w q" delete-window :name "close window")
 ("w k" windmove-left :name "focus left")
 ("w n" windmove-down :name "focus down")
 ("w e" windmove-up :name "focus up")
 ("w i" windmove-right :name "focus right")
 ("w u" winner-undo :name "undo window config")
 ("w U" winner-redo :name "redo window config")
 ("w w" "C-x o" :name "other window"))

;; implement modal editing using text object/operator relationship
(let ((ryo-text-obj
       '(;; single char style text object
         ("l" mark-word :name "to end of word")
         )))
  (eval `(ryo-modal-keys
          ;; basic operators
          ("v" ,ryo-text-obj)
          ("c" ,ryo-text-obj :then '(kill-region) :exit t)
          ("d" ,ryo-text-obj :then '(kill-region))
          ("y" ,ryo-text-obj :then '(copy-region-as-kill)))))

(provide 'hge-ryo-bindings)
