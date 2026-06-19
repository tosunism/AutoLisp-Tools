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
  (if (null sample)
    (getSample)
  )
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
          (cons 1 (strcat prefix (formatNumber number)))
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
            (cons 1 (strcat prefix (formatNumber number)))
            (assoc 1 target)
            target
          )
        )
      )
      ((= modifyMode "B")
        (entmod
          (subst
            (cons 1 (strcat prefix (formatNumber number)))
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
(setq digits 0)
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
      (setq choice (strcase (getstring "\nHit enter to continue or [S]ettings")))
      (if (= choice "S")
        (progn
          (changeSettings)
          (setq mode nil
            addMode nil
            modifyMode nil
            sample nil
            digits nil
          )
        )
      )
    )
  )
  (while (not mode)
    (initget "A M S")
    (setq mode (getkword "\nSelect mode [Add/Modify] or [S]ettings <Add>: "))
    (if (null mode)
      (setq mode "A")
    )
    (if (= mode "S")
      (changeSettings)
      (setq mode nil)
    )
  )
  (cond   
    ((= mode "A")
      (addNumber)
    )
    ((= mode "M")
      (modifyNumber)
    )
  )
)

(defun changeSettings ( / choice)
  (initget "N P")
  (setq choice (getkword "\nSet number or prefix [Number/Prefix] <Number>: "))
  (if (null choice)
    (setq choice "N")
  )
  (cond
    ((= choice "N")
      (setq number (getstring "\nEnter the starting number"))
      (setq digits (strlen number ))
      (setq number (atoi number))
      (setq mode nil)
    )
    ((= choice "P")
      (setq prefix (getstring T "\nEnter the prefix or hit Enter to continue: "))
      (setq mode nil)
    )
  )
)

(defun formatNumber (num / lenNum i diff )
  (setq num (itoa num))
  (setq lenNum (strlen num))
  (if (< lenNum digits)
    (progn
      (setq diff (- digits lenNum))
      (setq i 0)
      (while (< i diff)
        (setq num (strcat "0" num))
        (setq i (1+ i))
      )
    )
  )
  num
)

(defun setAttribute (blockRef / attrs attName)
  (setq attrs (vlax-invoke blockRef 'GetAttributes)) ;'
  (setq attName (cdr (assoc 2 sample)))
  (foreach att attrs
    (if (= (strcase (vla-get-TagString att))
        (strcase attName))
      (vla-put-TextString
        att
        (strcat prefix (formatNumber number))
      )
    )
  )
)

(defun getTarget ( / target ent)
  (while (null target)
    (initget "U")
    (if (= mode "A")  
      (setq target (getpoint "\nClick to add or [U]ndo <Esc to exit>: "))
      (progn
        (setq ent (getInput))
        (if (= ent "U")
          (setq target "U")
          (progn
            (setq target ent)
            (cond
              ((= modifyMode "T")
                (if (/= (cdr (assoc 0 ent)) "TEXT")
                  (progn
                    (princ "\nNot a text object")            
                    (setq target nil)
                  )
                )
              )
              ((= modifyMode "B")
                (if (/= (cdr (assoc 0 ent)) "ATTRIB")
                  (progn
                    (princ "\nNot an attribute object")            
                    (setq target nil)
                  )
                )
              )
            )
          )
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

(defun getInput ( / entity temp)
  (while (not entity)
    (initget "U")
    (setq temp (nentsel "Pick the object to number or [U]ndo"))
    (cond
      ((listp temp)
        (setq entity (entget (car temp)))
      )
      ((= temp "U")
        (setq entity "U")
      )
    )
  )
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