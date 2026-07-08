(vl-load-com)
(setq doc
  (vla-get-ActiveDocument
    (vlax-get-acad-object)
  )
)
(setq tagName nil)
(setq attBlockName nil)
(setq poLayer nil)

(defun changeSettings ( / attEnt attBlock polEnt)
  (setq attEnt (entget (car (nentsel "\nSelect the area attribute"))))
  (setq tagName (cdr (assoc 2 attEnt)))
  (setq attBlock (cdr (assoc 330 attEnt)))
  (setq attBlockName (vla-get-effectivename (vlax-ename->vla-object attBlock)))
  (setq polEnt (entget (car (entsel "\nSelect the polyline layer"))))
  (setq poLayer (cdr (assoc 8 polEnt)))
)

(defun c:TagUpdate ( / polSet entPol points i j afield blockSS block blkObj atts found )
  (if (not tagName)
    (changeSettings)
    (progn
      (initget "S")
      (if (getkword "\nSelect polylines or Settings <Select>: ")
        (changeSettings)
      )
    )
  )
  (setq polSet (ssget '((0 . "LWPOLYLINE"))))
  (setq i 0)
  (repeat (sslength polSet)
    (setq entPol (ssname polSet i))
    (if (= (cdr (assoc 8 (entget entpol))) poLayer)
      (progn
        (setq afield (GetAreaField (vlax-ename->vla-object entPol)))
        (setq points (GetPolylineVertices entPol))
        (princ "\n")
        (setq blockSS (ssget "_WP" points '((0 . "INSERT"))) );'
        (if blockSS
          (progn
            (setq j 0)
            (setq found nil)
            (while (and (< j (sslength blockSS)) (not found))
              (setq block (ssname blockSS j))
              (setq blockObj (vlax-ename->vla-object block))
              (if (= (vla-get-effectivename blockObj) attBlockName)
                (progn
                  (setq found T)
                  (setq atts
                    (vlax-invoke blockObj 'GetAttributes)
                  )
                  (foreach att atts
                    (if
                      (= (strcase (vla-get-TagString att))
                          (strcase tagName))
                      (progn
                        (vla-put-TextString att afield)
                        (vla-Update att)
                        (command "_.UPDATEFIELD" block "")
                      )
                    )
                  )
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