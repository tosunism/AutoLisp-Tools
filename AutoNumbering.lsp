(defun undo (mode / prevEntity prevContent)
  (if numbered
    (progn
      (cond
        ((= mode "A")
          (entdel (car numbered))      
        )
        ((= mode "M")
          (setq prevEntity (entget (car (car numbered))))
          (setq prevContent (cdr (car numbered)))
          (entmod
            (subst
              (cons 1 prevContent) 
              (assoc 1 prevEntity)
              prevEntity
            )
          )
        )
      )
      (setq numbered (cdr numbered))
      (setq number (1- number))
    )
    (princ "\nNothing to undo.")
  )
)

(defun addNumber ( / target)
  (if (null addMode)
    (progn
      (initget "T B")
      (setq addMode (getkword "\nSelect adding mode [Text/Block] <Text>:"))
      (if (null addMode)
        (setq addMode "T")
      )
    )
  )
  (getSample)
  (while T
    (setq target (getTarget))
    (cond
      ((= addMode "T")
        (entmake
          (list
          (cons 0 "TEXT")
          (cons 8 (cdr (assoc 8 sample)))
          (cons 10 target)
          (cons 40 (cdr (assoc 40 sample)))
          (cons 1 (strcat prefix (itoa number)))
          (cons 7 (cdr (assoc 7 sample)))
          )
        )        
        (setq numbered (cons (entlast) numbered))
        (setq number (1+ number))
      )
      ((= addMode "B")
        (setq bObj
          (vlax-ename->vla-object
            (cdr (assoc 330 sample))
          )
        )
        (setq bName (vla-get-EffectiveName bObj))
        (setq bRef
          (vla-InsertBlock
            ms
            (vlax-3d-point target)
            bName
            (vla-get-XScaleFactor bObj)
            (vla-get-YScaleFactor bObj)
            (vla-get-ZScaleFactor bObj)
            (vla-get-Rotation bObj)
          )
        )
        (setAttribute bRef)
        (setq numbered
          (cons
            (vlax-vla-object->ename bRef)
            numbered
          )
        )
        (setq number (1+ number))
      )
    )
  )
)

(defun modifyNumber ( / target)
  (if (null modifyMode)
    (progn
      (initget "T B")
      (setq modifyMode (getkword "\nSelect modify mode [Text/Block] <Text>:"))
      (if (null modifyMode)
        (setq modifyMode "T")
      )
    )
  )
  (while T
    (setq target (getTarget))
    (cond
      ((= modifyMode "T")
        (entmod
          (subst
            (cons 1 (strcat prefix (itoa number)))
            (assoc 1 target)
            target
          )
        )
      )
      ((= modifyMode "B")
        (entmod
          (subst
            (cons 1 (strcat prefix (itoa number)))
            (assoc 1 target)
            target
          )
        )
      )
    )
    (setq numbered
          (cons
            (cons
              (cdr (assoc -1 target))
              (cdr (assoc 1 target))
            )
            numbered
          )
    )
    (setq number (1+ number))
  )  
)

(setq prefix "")
(setq mode nil)
(setq addMode nil)
(setq modifyMode nil)
(setq number 1)
(setq numbered nil)
(setq sample nil)
(vl-load-com)
(setq doc
  (vla-get-ActiveDocument
    (vlax-get-acad-object)
  )
)
(setq ms
  (vla-get-ModelSpace doc)
)

(defun c:numAuto ( / choice)
  (if mode
    (progn
      (initget "S")
      (setq choice (getkword "\nHit enter to continue or [S]ettings"))
      (if (= choice "S") (setq mode nil))
    )
  )
  (if (not mode)  
    (progn
      (initget "A M P N")
      (setq mode (getkword "\nSelect mode [Add/Modify/Prefix/Number] <Add>: "))
      (if (null mode)
        (setq mode "A")
      )
    )
  )
  (cond   
      ((= mode "A")
        (addNumber)
      )
      ((= mode "M")
        (modifyNumber)
      )
      ((= mode "P")
        (setq prefix (getstring "\nEnter the prefix or hit Enter to continue: "))
        (setq mode nil)
        (c:numAuto)
      )
      ((= mode "N")
        (setq number (atoi (getstring "\nEnter the starting number")))
        (setq mode nil)
        (c:numAuto)
      )
    )
)

(defun setAttribute (blockRef / attrs attName)
  (setq attrs (vlax-invoke blockRef 'GetAttributes)) ;'
  (setq attName (cdr (assoc 2 sample)))
  (foreach att attrs
    (if (= (strcase (vla-get-TagString att))
        (strcase attName))
      (vla-put-TextString
        att
        (strcat prefix (itoa number))
      )
    )
  )
)
(defun getTarget ( / target ent)
  (while (null target)
    (initget "U")
    (if (= mode "A")  
      (setq target (getpoint "\nClick to add [U]ndo <Esc to exit>: "))
      (cond
        ((= modifyMode "T")
          (setq ent (entget (car (entsel "\nPick the text to number: "))))
          (if (= (cdr (assoc 0 ent)) "TEXT")
            (setq target ent) 
            (progn
              (princ "\nNot a text object")            
              (setq target nil)
            )
          )
        )
        ((= modifyMode "B")
          (setq target (nentsel "\nPick a block attribute: "))
        )
      )
    )
    (cond
      ((null target) (princ "\nNo selection"))
      ((= target "U")
        (undo mode)
        (setq target nil)
      )
    )
  )
  target
)

(defun getSample ( / src)
  (setq sample nil)
  (while (not sample)
    (cond 
      ((= addMode "T") 
        (setq src (entsel "\nSelect a sample text"))
      )
      ((= addMode "B") 
        (setq src (nentsel "\nSelect a sample attributed block"))
      )
    )      
    (if src
      (progn
        (setq sample (entget (car src)))
        (cond
          ((= addMode "T")
            (if (/= (cdr (assoc 0 sample)) "TEXT")
              (progn      
                (princ "\nNot a text object")
                (setq sample nil)
              )
            )
          )
          ((= addMode "B")
            (if (/= (cdr (assoc 0 sample)) "ATTRIB")
              (progn
                (princ "\nNot an attribute object")
                (setq sample nil)
              )
            )
          )
        )
      )
      (princ "\nNo selection")
    )
  )
  sample
)