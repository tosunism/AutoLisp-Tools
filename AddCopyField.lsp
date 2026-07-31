(vl-load-com)
(setq doc
  (vla-get-ActiveDocument
    (vlax-get-acad-object)
  )
)

(setq ArAttName nil) ; area attribute
(setq AttBlockName nil) ; name of the block that contains the attributes
(setq PLayer nil)

(defun changeSettings ( / attEnt attBlock)
  (setq attEnt (entget (car (nentsel "\nSelect the area attribute"))))
  (setq PLayer (cdr (assoc 8 (entget (car (nentsel "\nSelect the area polyline"))))))
  (setq ArAttName (cdr (assoc 2 attEnt)))
  (setq attBlock (cdr (assoc 330 attEnt)))
  (setq AttBlockName (vla-get-effectivename (vlax-ename->vla-object attBlock)))
)

(defun c:AddAreaField ( / selSet entPol points i j afield blockSS block blockObj found srcBkSS choice sNam)
  (if (not ArAttName)
    (changeSettings)
  )
  (initget "S")
  (setq choice T)
  (while choice
    (setq choice (getkword "\nPress enter to select polylines or [Settings] <Select>:"))
    (if (= choice "S") (changeSettings))
  )
  (setq selSet (ssget '((0 . "LWPOLYLINE")))) ;'
  (setq i 0)
  (setq srcBkSS (ssadd))
  (repeat (sslength selSet)
    (setq sNam (ssname selSet i))
    (setq entPol (entget sNam))
    (if (= (cdr (assoc 8 entPol)) PLayer)
      (progn
        (setq afield (GetAreaField (vlax-ename->vla-object sNam)))
        (setq points (GetPolylineVertices sNam))
        (setq blockSS (ssget "_WP" points '((0 . "INSERT"))));'
        (if blockSS
          (progn
            (setq j 0)
            (setq found nil)
            (while (and (< j (sslength blockSS)) (not found))
              (setq block (ssname blockSS j))
              (setq blockObj (vlax-ename->vla-object block))
              (if (= (vla-get-effectivename blockObj) AttBlockName)
                (progn
                  (setq found T)
                  (ssadd block srcBkSS)
                  (putFieldToAttribute block ArAttName afield)
                )
              )
              (setq j (1+ j))
            )
          )
        )
      )
    )
    (setq i (1+ i))
  )
  (command "_.UPDATEFIELD" srcBkSS "")
  (princ)
)

(defun GetPolylineVertices (ent / lst pts)
  (setq lst (vlax-safearray->list
              (vlax-variant-value
                (vla-get-Coordinates (vlax-ename->vla-object ent)))))
  (while lst
    (setq pts (cons (list (car lst) (cadr lst)) pts)
          lst (cddr lst)
    )
  )
  pts
)

(defun GetAreaField (object)
  (strcat
    "%<\\AcObjProp Object(%<\\_ObjId "
    (itoa (vla-get-ObjectID object))
    ">%).Area \\f \"%lu2%pr2\">%"
  )
)

(defun putFieldToAttribute (bkNam atNam content / atts)
  (setq bkObj (vlax-ename->vla-object bkNam))
  (setq atts (vlax-invoke bkObj 'GetAttributes)) ;'
  (foreach att atts
    (if
      (= (strcase (vla-get-TagString att))
          (strcase atNam))
      (progn
        (vla-put-TextString att content)
      )
    )
  )
)

;; CopyAtrributeValue functions

(setq FilterAttName nil)  ; reference attribute name e.g. room
(setq AtNamToCp nil)  ; attribute name to copy e.g. area attribute
(setq CpAttBlockName nil)  ; name of the block that contains the attributes

(defun c:CopyFldToAtts ( / fld attObj stag bkSet tBkNam i sBkNam)
  (if 
    (and
      (setq sBkNam (car (nentsel "\nSelect a source attribute to copy from")))
      (= "ATTRIB" (cdr (assoc 0 (entget sBkNam))))
      (setq fld (LM:fieldcode sBkNam))
    )
    (progn
      (setq attObj (vlax-ename->vla-object sBkNam))
      (setq stag (strcase (vla-get-tagstring attObj)))
      (princ "\nSelect target blocks to copy to")    
      (setq bkSet (ssget '((0 . "INSERT")))) ;'
      (if bkSet  
        (progn
          (setq i 0)
          (repeat (sslength bkSet)
            (setq tBkNam (ssname bkSet i))
            (putFieldToAttribute tBkNam stag fld)
            (setq i (1+ i))
          )
          (command "_.UPDATEFIELD" bkSet "")          
        )
      )
    )
  )
  (prompt "\nNot a fielded attribute")
  (princ)
)

(defun c:CopyBlockAttValues ( / sBkNam valMap)
  (setq sBkNam (car (entsel "\nSelect a source block to copy attribute values from")))
  (if (and sBkNam (= (cdr (assoc 0 (entget sBkNam))) "INSERT"))
    (progn
      (setq valMap (buildAttValueMap sBkNam))
      (setq sBkEffNam (vla-get-effectivename (vlax-ename->vla-object sBkNam)))
    )
    (progn
      (prompt "\nNot a block")
      (setq valMap nil)
    )
  )
  (if valMap
    (progn
      (princ "\nSelect target blocks to copy to")
      (setq tarBkSet (ssget '((0 . "INSERT")))) ;'
      (if tarBkSet
        (progn
          (setq i 0)
          (repeat (sslength tarBkSet)
            (setq tBkNam (ssname tarBkSet i))
            (setq tBkEffNam (vla-get-effectivename (vlax-ename->vla-object tBkNam)))
            (if (= tBkEffNam sBkEffNam)
              (foreach pair valMap
                (putFieldToAttribute tBkNam (car pair) (cdr pair))
              )
            )            
            (setq i (1+ i))
          )
          (command "_.UPDATEFIELD" tarBkSet "")
        )
      )
    )
  )
)

(defun buildAttValueMap (bkNam / bkObj atts att fld aTag aVal map)
  (setq bkObj (vlax-ename->vla-object bkNam))
  (setq atts (vlax-invoke bkObj 'GetAttributes)) ;'
  (while atts
    (setq att (car atts))
    (setq aTag (vla-get-TagString att))
    
    (setq fld
      (vl-catch-all-apply
        'LM:fieldcode
        (list (vlax-vla-object->ename att))
      )
    )
    (if (vl-catch-all-error-p fld)
      (setq fld nil)      
    )
    
    (if fld
      (setq aVal fld)
      (setq aVal (vla-get-TextString att))
    )    
    (setq map (cons (cons aTag aVal) map))
    (setq atts (cdr atts))
  )
  map
)

(defun c:CopyAttributeValueByFilter ( / cont attObj pair tarBkSet targetBk room i choice attFldMap filter)
  (setq choice T)
  (while choice
    (initget "R")
    (setq choice (getkword "\nPress enter to select objects or [Reset]"))
    (if (= choice "R") (progn (setq AtNamToCp nil) (setq FilterAttName nil)))
  )
  (if (or (not AtNamToCp) (not FilterAttName))
    (chgCpAttSettings)
  )
  (princ "\nSelect source blocks to copy from")
  (setq filter '((0 . "INSERT") (66 . 1)))
  (setq srcBkSS (ssget filter))
  (filterBlocksByName srcBkSS CpAttBlockName)
  (setq attFldMap (buildAttributeMap srcBkSS))
  (princ "\nSelect target blocks to copy to")
  (setq tarBkSet (ssget filter))
  (filterBlocksByName tarBkSet CpAttBlockName)
  (setq i 0)
  (repeat (sslength tarBkSet)
    (setq targetBk (ssname tarBkSet i))
    (setq room (getAttributeContent FilterAttName targetBk))
    (setq pair (assoc room attFldMap))
    (if pair (putFieldToAttribute targetBk AtNamToCp (cdr pair)))
    (setq i (1+ i))
  )
  (command "_.UPDATEFIELD" tarBkSet "")
)
(defun filterBlocksByName (ss blkName / newSS i bk)
  (setq newSS (ssadd))
  (setq i 0)
  (repeat (sslength ss)
    (setq bk (ssname ss i))
    (if (= (strcase (vla-get-EffectiveName (vlax-ename->vla-object bk)))
           (strcase blkName))
      (ssadd bk newSS)
    )
    (setq i (1+ i))
  )
  newSS
)
(defun getAttributeContent (atNam bkNam / atts att content)
  (setq bkObj (vlax-ename->vla-object bkNam))
  (setq atts (vlax-invoke bkObj 'GetAttributes)) ;'
  (while atts
    (setq att (car atts))
    (if (= (strcase (vla-get-TagString att))
       (strcase atNam))
      (setq content (vla-get-TextString att))
    )
    (setq atts (cdr atts))
  )
  content
)

(defun buildAttributeMap (bSet / i bk fAttVal cAttObj cAttFld map)
  (setq i 0)
  (repeat (sslength bSet)
    (setq bk (ssname bSet i))
    (setq fAttVal (getAttributeContent FilterAttName bk))
    (setq cAttObj (getAttributeObject AtNamToCp bk))
    (if cAttObj
      (setq cAttFld
            (LM:fieldcode (vlax-vla-object->ename cAttObj)))
      (setq cAttFld nil)
    )
    (if fAttVal
      (setq map (cons (cons fAttVal cAttFld) map))
    )
    (setq i (1+ i))
  )
  map
)
(defun getAttributeObject (atNam bkNam / atts att obj)
  (setq obj (vlax-ename->vla-object bkNam))
  (setq atts (vlax-invoke obj 'GetAttributes))
  (foreach att atts
    (if (= (strcase (vla-get-TagString att))
           (strcase atNam))
      (setq obj att)
    )
  )
  obj
)
(defun chgCpAttSettings ( / sAtEnt tAtEnt attBlock )
  (setq sAtEnt (entget (car (nentsel "\nSelect an attribute to copy"))))
  (setq AtNamToCp (cdr (assoc 2 sAtEnt)))
  (setq attBlock (cdr (assoc 330 sAtEnt)))
  (setq CpAttBlockName (vla-get-EffectiveName (vlax-ename->vla-object attBlock)))
  (setq tAtEnt (entget (car (nentsel "\nSelect an attribute to filter"))))
  (setq FilterAttName (cdr (assoc 2 tAtEnt)))
)
(princ)



;; Field Code  -  Lee Mac
;; Returns the field expression associated with an entity

(defun LM:fieldcode ( ent / replacefield replaceobject fieldstring enx )
 
    (defun replacefield ( str enx / ent fld pos )
        (if (setq pos (vl-string-search "\\_FldIdx" (setq str (replaceobject str enx))))
            (progn
                (setq ent (assoc 360 enx)
                      fld (entget (cdr ent))
                )
                (strcat
                    (substr str 1 pos)
                    (replacefield (fieldstring fld) fld)
                    (replacefield (substr str (1+ (vl-string-search ">%" str pos))) (cdr (member ent enx)))
                )
            )
            str
        )
    )
 
    (defun replaceobject ( str enx / ent pos )
        (if (setq pos (vl-string-search "ObjIdx" str))
            (strcat
                (substr str 1 (+ pos 5)) " "
                (LM:ObjectID (vlax-ename->vla-object (cdr (setq ent (assoc 331 enx)))))
                (replaceobject (substr str (1+ (vl-string-search ">%" str pos))) (cdr (member ent enx)))
            )
            str
        )
    )
 
    (defun fieldstring ( enx / itm )
        (if (setq itm (assoc 3 enx))
            (strcat (cdr itm) (fieldstring (cdr (member itm enx))))
            (cond ((cdr (assoc 2 enx))) (""))
        )
    )
    
    (if (and (wcmatch  (cdr (assoc 0 (setq enx (entget ent)))) "TEXT,MTEXT,ATTRIB,MULTILEADER,*DIMENSION")
             (setq enx (cdr (assoc 360 enx)))
             (setq enx (dictsearch enx "ACAD_FIELD"))
             (setq enx (dictsearch (cdr (assoc -1 enx)) "TEXT"))
        )
        (replacefield (fieldstring enx) enx)
    )
)
 
;; ObjectID  -  Lee Mac
;; Returns a string containing the ObjectID of a supplied VLA-Object
;; Compatible with 32-bit & 64-bit systems
 
(defun LM:ObjectID ( obj )
    (eval
        (list 'defun 'LM:ObjectID '( obj ) 
            (if
                (and
                    (vl-string-search "64" (getenv "PROCESSOR_ARCHITECTURE"))
                    (vlax-method-applicable-p (vla-get-utility (LM:acdoc)) 'getobjectidstring)
                )
                (list 'vla-getobjectidstring (vla-get-utility (LM:acdoc)) 'obj ':vlax-false)
               '(itoa (vla-get-objectid obj))
            )
        )
    )
    (LM:ObjectID obj)
)