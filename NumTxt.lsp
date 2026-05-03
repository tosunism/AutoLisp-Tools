(defun getTxt ( prStr / sampList src lay hgt sty content txtEnt)
  (setq sampList nil)
  (setq quit nil)
  (while ( and (not sampList) (not quit))
    (setq src (entsel prStr))
    (if src
      (progn
        (setq txtEnt (entget (car src)))
        (if (= (cdr (assoc 0 txtEnt)) "TEXT")
          (progn
            (setq lay (cdr (assoc 8 txtEnt)))
            (setq hgt (cdr (assoc 40 txtEnt)))
            (setq sty (cdr (assoc 7 txtEnt)))
            (setq content (cdr (assoc 1 txtEnt)))
            (setq sampList (list lay hgt sty content txtEnt))
          )
          (princ "\nNot a TEXT object.")
        )
      )
      (setq quit T)
    )
  )
  (if null sampList    
    (princ "\nCancelled.")
    sampList
  )
)

(defun c:numTxt ( / sampLs num numbered pt)
  (setq sampLs (getTxt "\nSelect the previous text"))
  (setq num (1+ (atoi (nth 3 sampLs))))
  (setq numbered nil)
  (while T
    (initget "U")
    (setq pt (getpoint "\nPick point or [U]ndo <Enter to finish>: "))
    (cond
      ((null pt)
        (exit)
      )
      ((= pt "U")
        (if numbered
          (progn
            (entdel (car numbered))
            (setq numbered (cdr numbered))
            (setq num (1- num))
          )
          (princ "\nNothing to undo.")
        )
      )
      (T
        (entmake
          (list
            '(0 . "TEXT")
            (cons 8  (nth 0 sampLs))
            (cons 10 pt)
            (cons 40 (nth 1 sampLs))
            (cons 1  (itoa num))
            (cons 7  (nth 2 sampLs))
          )
        )
        (setq numbered (cons (entlast) numbered))
        (setq num (1+ num))
      )
    )
  )
  (princ)
)

(defun c:UpdNumTxt (/ sampLs num numbered pt)
  (setq sampLs (getTxt "\nSelect the previous text"))
  (setq num (1+ (atoi (nth 3 sampLs))))
  (while T
    (setq txtToNum (getTxt "\nPick the text to number"))
    (if txtToNum
      (progn
        (setq txtEnt (nth 4 txtToNum))
        (entmod
          (subst (cons 1 (itoa num)) (assoc 1 txtEnt) txtEnt)        
        )
        (setq num (1+ num))
      )
    )
  )
  (princ)
)
(defun c:UpdNumTxt2 (number sample)
  (setq txtToNum (getTxt "\nPick the text to number"))
  (if txtToNum
    (progn
      (setq txtEnt (nth 4 txtToNum))
      (entmod (subst (cons 1 (itoa number) (assoc 1 txtEnt))))
    )
  )
)

; getPrevious a
; undo a b c
; numberObj while a b c