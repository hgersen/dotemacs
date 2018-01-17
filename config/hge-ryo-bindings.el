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
 ("w" forward-to-word :name "forward to word")
 ("b" "M-b" :name "backward to word")
 ("E" "M-{" :name "next paragraph")
 ("N" "M-}" :name "prev paragraph"))

;; modal insert editing maps
(ryo-modal-keys
 ("s" my/disable-ryo-modal-mode :name "insert here")
 ("a" my/move-start-of-line :name "add at start of line" :exit t)
 ("A" move-end-of-line :name "add at end of line" :exit t)
 ("o" my/open-line-below :name "open line below" :exit t)
 ("O" my/open-line-above :name "open line above" :exit t))

;; modal editing
(ryo-modal-keys
 (:norepeat t)
 ("p" "C-y" :name "paste")
 ("r" delete-char :name "remove char")
 ("R" backward-delete-char-untabify :name "remove char backwards")
 ("d d" kill-whole-line :name "delete whole line"))

;; window navigation
(ryo-modal-keys
 (:norepeat t)
 ("v o" switch-to-buffer-other-window :name "switch other window")
 ("v c" delete-window :name "close window")
 ("v k" windmove-left :name "focus left")
 ("v n" windmove-down :name "focus down")
 ("v e" windmove-up :name "focus up")
 ("v i" windmove-right :name "focus right")
 ("v u" winner-undo :name "undo window config")
 ("v U" winner-redo :name "redo window config")
 ("v v" "C-x o" :name "other window"))

;; implement modal editing using text object/operator relationship
(let ((ryo-text-obj
       '(;; single char style text object
         ("k" my/mark-backward-char :name "previous char(s)")
         ("i" my/mark-forward-char :name "next char(s)")
         ("b" my/mark-backward-word :name "to start of word")
         ("w" my/mark-forward-word :name "to end of word")
         ("e" my/mark-backward-line :name "to previous line")
         ("n" my/mark-forward-line :name "to next line")
         ("E" my/mark-backward-paragraph :name "to start of paragraph")
         ("N" my/mark-forward-paragraph :name "to end of paragraph")
         )))
  (eval `(ryo-modal-keys
          ;; basic operators
          ("f" ,ryo-text-obj)
          ("c" ,ryo-text-obj :then '(kill-region) :exit t)
          ("d" ,ryo-text-obj :then '(kill-region))
          ("y" ,ryo-text-obj :then '(copy-region-as-kill)))))

(provide 'hge-ryo-bindings)
