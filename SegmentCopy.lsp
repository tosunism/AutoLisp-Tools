(defun c:SegCopy ( / sel entName pick matrix obj vindex p1 p2 param plist)
  (vl-load-com)
  (setq done nil)
  (while (not done)
    (setq sel (nentselp "\nPick polyline segment: "))
    (if sel 
      (progn
        (setq entName (car sel)
              pick (cadr sel)
              matrix (caddr sel)
        )
        (setq pick (trans pick 1 0))      
        (if matrix
          (setq pick (MatrixInverseTransformPoint pick matrix))
        )
        
        (setq obj (vlax-ename->vla-object entName))  
        (setq objName (vla-get-ObjectName obj))
        ; Start-End Point extraction
        (cond
          ((= objName "AcDbLine")
            (setq p1 (vlax-curve-getstartpoint obj))
            (setq p2 (vlax-curve-getendpoint obj))

          )
          ((= objName "AcDbPolyline")
            (setq pointOnCurve (vlax-curve-getclosestpointto obj pick))
            (setq param (vlax-curve-getParamAtPoint obj pointOnCurve))
            (setq vindex (fix param))
            (setq p1 (vlax-curve-getpointatparam obj vindex))
            (setq p2 (vlax-curve-getpointatparam obj (1+ vindex)))
          )
        )
        (if matrix
          (progn
            (setq p1 (MatrixTransformPoint p1 matrix))
            (setq p2 (MatrixTransformPoint p2 matrix))
          )
        )
        ; Object creation 
        (entmake
          (list
            (cons 0 "LWPOLYLINE")
            (cons 100 "AcDbEntity")
            (cons 100 "AcDbPolyline")
            (cons 90 2)
            (cons 70 0)
            (cons 10 p1)
            (cons 10 p2)
          )
        )
        (setq plist
          (cons
            (list (entlast) p1 p2)
            plist
          )
        )      
      )
      (progn        
        (if plist
          (progn
            ;; create final joined polylines
            (foreach chain (ChainSegments plist)
              (MakePline chain)
            )
            ;; remove preview polylines
            (foreach seg plist
              (entdel (car seg))
            )
          )
        )
        (setq done T)
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

(defun SamePoint (a b /)
  (equal a b 1e-3)
)

(defun ChainSegments (segs / chains chain seg rest found)
  
  (while segs

    ;; start a new chain with first remaining segment
    (setq chain
      (list
        (cadr (car segs))
        (caddr (car segs))
      )
    )

    (setq segs (cdr segs))
    (setq found T)

    ;; keep searching for connected segments
    (while found

      (setq found nil)
      (setq rest segs)

      (while rest

        (setq seg (car rest))

        (cond

          ;; connect to chain end
          ((SamePoint (car (last chain))
                      (cadr seg))

            (setq chain
              (append chain (list (caddr seg)))
            )

            (setq segs (vl-remove seg segs))
            (setq found T)
          )


          ;; reversed segment connects to chain end
          ((SamePoint (car (last chain))
                      (caddr seg))

            (setq chain
              (append chain (list (cadr seg)))
            )

            (setq segs (vl-remove seg segs))
            (setq found T)
          )


          ;; connect to chain start
          ((SamePoint (car chain)
                      (caddr seg))

            (setq chain
              (cons (cadr seg) chain)
            )

            (setq segs (vl-remove seg segs))
            (setq found T)
          )


          ;; reversed segment connects to chain start
          ((SamePoint (car chain)
                      (cadr seg))

            (setq chain
              (cons (caddr seg) chain)
            )

            (setq segs (vl-remove seg segs))
            (setq found T)
          )
        )

        (setq rest (cdr rest))
      )
    )

    ;; save completed chain
    (setq chains
      (cons chain chains)
    )
  )

  chains
)

(defun MakePline (pts / data)
  (setq data
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbPolyline")
      (cons 90 (length pts))
      '(70 . 0)
    )
  )

  (foreach p pts
    (setq data
      (append data
        (list
          (cons 10 (list (car p) (cadr p)))
        )
      )
    )
  )

  (entmake data)
)