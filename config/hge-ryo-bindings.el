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
 ("b" "M-b" :name "backward to word"))

;; modal insert editing maps
(ryo-modal-keys
 ("o" my/disable-ryo-modal-mode :name "insert")
 ("O" my/move-beginning-of-line :name "insert at start of line" :exit t)
 ("h" my/open-line-below :name "open line below" :exit t)
 ("H" my/open-line-above :name "open line above" :exit t)
 ("a" forward-char :name "append" :exit t)
 ("A" move-end-of-line :name "append end of line" :exit t))

;; modal editing
(ryo-modal-keys
 (:norepeat t)
 ("p" "C-y" :name "paste")
 ("x" delete-char :name "delete char"))

;; undo/redo
(ryo-modal-keys
 ("u" "C-/" :name "undo")
 ("U" "M-/" :name "repeat"))

(provide 'hge-ryo-bindings)
