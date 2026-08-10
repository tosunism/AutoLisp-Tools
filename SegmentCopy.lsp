(setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
(vl-load-com)

(setq joinedEnt nil)
(setq *SgCopyOffsetReactor* nil)


(defun c:SgCopy ( / sel entName pick matrix obj objName
                    vindex p1 p2 param pointOnCurve
                    bulge selSet )

  (setq selSet (ssadd))

  (while (setq sel (nentselp "\nPick a segment : "))

    (setq entName (car sel)
          pick    (cadr sel)
          matrix  (caddr sel))

    (setq pick (trans pick 1 0))

    (if matrix
      (setq pick (MatrixInverseTransformPoint pick matrix))
    )

    (setq obj     (vlax-ename->vla-object entName)
          objName (vla-get-ObjectName obj)
          bulge   0.0)

    ;; Start-End Point extraction
    (cond
      ((= objName "AcDbLine")
        (setq p1 (vlax-curve-getstartpoint obj)
              p2 (vlax-curve-getendpoint obj))
      )

      ((= objName "AcDbPolyline")
        (setq pointOnCurve
               (vlax-curve-getclosestpointto obj pick))

        (setq param
               (vlax-curve-getParamAtPoint obj pointOnCurve))

        (setq vindex (fix param))

        (setq bulge
               (vla-GetBulge obj vindex))

        (if (and matrix (MatrixMirroredP matrix))
          (setq bulge (- bulge))
        )

        (setq p1
               (vlax-curve-getPointAtParam obj vindex))

        (setq p2
               (vlax-curve-getPointAtParam obj (1+ vindex)))
      )
    )

    ;; MCS -> WCS
    (if matrix
      (progn
        (setq p1 (MatrixTransformPoint p1 matrix))
        (setq p2 (MatrixTransformPoint p2 matrix))
      )
    )

    ;; Create copied segment
    (entmake
      (list
        (cons 0 "LWPOLYLINE")
        (cons 100 "AcDbEntity")
        (cons 100 "AcDbPolyline")
        (cons 90 2)
        (cons 70 0)
        (cons 10 p1)
        (cons 42 bulge)
        (cons 10 p2)
      )
    )

    (ssadd (entlast) selSet)
  )

  ;; ----------------------------------------------------------
  ;; Start JOIN / OFFSET
  ;; ----------------------------------------------------------

  (if (> (sslength selSet) 0)
    (progn

      (sssetfirst nil selSet)

      (if (> (sslength selSet) 1)

        (vla-SendCommand
          doc
          "_.JOIN\nOffsetJoined\n"
        )

        (vla-SendCommand
          doc
          "OffsetJoined\n"
        )
      )
    )
  )

  (princ)
)


(defun c:OffsetJoined ( / ent ss )

  ;; The object currently being used for OFFSET
  (setq ent (entlast))

  (if ent
    (progn

      ;; Save it globally
      (setq joinedEnt ent)

      (princ
        (strcat
          "\nJoined entity: "
          (vl-princ-to-string joinedEnt)
        )
      )

      ;; Put it in PickFirst
      (setq ss (ssadd))
      (ssadd ent ss)
      (sssetfirst nil ss)

      ;; ------------------------------------------------------
      ;; Create reactor BEFORE starting OFFSET
      ;; ------------------------------------------------------

      (if *SgCopyOffsetReactor*
        (vlr-command-reactor
          *SgCopyOffsetReactor*
        )
      )

      (setq *SgCopyOffsetReactor*
        (vlr-command-reactor
          nil
          '((:vlr-commandEnded . SgCopy-OffsetEnded))
        )
      )

      ;; ------------------------------------------------------
      ;; Start OFFSET
      ;; ------------------------------------------------------

      (vla-SendCommand
        doc
        "_.OFFSET\nT\n"
      )
    )
  )

  (princ)
)


(defun SgCopy-OffsetEnded
  (reactor params / cmd)

  (setq cmd (strcase (car params)))

  (if (= cmd "OFFSET")

    (progn

      ;; Remove reactor first
      (if *SgCopyOffsetReactor*
        (progn
          (vlr-remove *SgCopyOffsetReactor*)
          (setq *SgCopyOffsetReactor* nil)
        )
      )

      ;; Delete the original joined object
      (if (and joinedEnt
               (entget joinedEnt))
        (progn
          (entdel joinedEnt)
          (setq joinedEnt nil)
        )
      )
    )
  )

  (princ)
)