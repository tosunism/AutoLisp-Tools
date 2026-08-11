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

; MCS → WCS
(defun MatrixTransformPoint (pt mat / r0 r1 r2 x y z)
  (setq r0 (nth 0 mat)
        r1 (nth 1 mat)
        r2 (nth 2 mat)
        x  (car pt)
        y  (cadr pt)
        z  (cond ((caddr pt)) (0.0)))

  (list
    (+ (* x (nth 0 r0)) (* y (nth 1 r0)) (* z (nth 2 r0)) (nth 3 r0))
    (+ (* x (nth 0 r1)) (* y (nth 1 r1)) (* z (nth 2 r1)) (nth 3 r1))
    (+ (* x (nth 0 r2)) (* y (nth 1 r2)) (* z (nth 2 r2)) (nth 3 r2))
  )
)

; WCS → MCS
(defun MatrixInverseTransformPoint (pt mat / r0 r1 r2
                                       a b c d e f g h i
                                       tx ty tz
                                       det
                                       inv00 inv01 inv02
                                       inv10 inv11 inv12
                                       inv20 inv21 inv22
                                       px py pz)

  (setq r0 (nth 0 mat)
        r1 (nth 1 mat)
        r2 (nth 2 mat))

  ;; Linear part
  (setq a (nth 0 r0)  b (nth 1 r0)  c (nth 2 r0)
        d (nth 0 r1)  e (nth 1 r1)  f (nth 2 r1)
        g (nth 0 r2)  h (nth 1 r2)  i (nth 2 r2))

  ;; Translation
  (setq tx (nth 3 r0)
        ty (nth 3 r1)
        tz (nth 3 r2))

  ;; Remove translation
  (setq px (- (car pt) tx)
        py (- (cadr pt) ty)
        pz (- (if (caddr pt) (caddr pt) 0.0) tz))

  ;; Determinant
  (setq det
    (+ (* a (- (* e i) (* f h)))
       (* (- b) (- (* d i) (* f g)))
       (* c (- (* d h) (* e g)))))

  (if (equal det 0.0 1e-12)
    nil
    (progn
      ;; Inverse 3×3
      (setq inv00 (/ (- (* e i) (* f h)) det)
            inv01 (/ (- (* c h) (* b i)) det)
            inv02 (/ (- (* b f) (* c e)) det)

            inv10 (/ (- (* f g) (* d i)) det)
            inv11 (/ (- (* a i) (* c g)) det)
            inv12 (/ (- (* c d) (* a f)) det)

            inv20 (/ (- (* d h) (* e g)) det)
            inv21 (/ (- (* b g) (* a h)) det)
            inv22 (/ (- (* a e) (* b d)) det))

      (list
        (+ (* px inv00) (* py inv01) (* pz inv02))
        (+ (* px inv10) (* py inv11) (* pz inv12))
        (+ (* px inv20) (* py inv21) (* pz inv22))
      )
    )
  )
)

(defun MatrixMirroredP (mat / r0 r1 r2
                            a b c d e f g h i det)

  (setq r0 (nth 0 mat)
        r1 (nth 1 mat)
        r2 (nth 2 mat))

  (setq a (nth 0 r0) b (nth 1 r0) c (nth 2 r0)
        d (nth 0 r1) e (nth 1 r1) f (nth 2 r1)
        g (nth 0 r2) h (nth 1 r2) i (nth 2 r2))

  (setq det
    (+ (* a (- (* e i) (* f h)))
       (* (- b) (- (* d i) (* f g)))
       (* c (- (* d h) (* e g)))))

  (< det 0.0)
)