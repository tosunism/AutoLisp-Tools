(defun c:AlOnSegment
  (/ ss ent1 ent2
     pt1 pt2 pt3 pt4
     parent parents
     ang1 ang2
     tol
     parallel
     dx dy
     perp dist srcPts tarPts)

  (setq tol 1e-3)

  ;; ------------------------------------------------------------
  ;; Select target object
  ;; ------------------------------------------------------------

  (setq ent2 (nentselp "\nSelect an object to align to"))

  (if ent2
    (progn      
      (setq ent1 (nentselp "\nSelect an object to align"))

      (if ent1
        (progn
          (setq ss (ssadd))

          ;; ------------------------------------------------------
          ;; Get parent

          (setq parents (cadddr ent1))
          (if parents
            (if (listp parents)
              (setq parent (last parents))
              (setq parent parents)
            )            
            (setq parent (car ent1))
          )
          (ssadd parent ss)
        ;; ------------------------------------------------------------
        ;; Get points
        ;; ------------------------------------------------------------
        (setq srcPts (getSegmentPoints ent1))
        (setq tarPts (getSegmentPoints ent2))
        (setq pt1 (car srcPts))
        (setq pt2 (car tarPts))
        (setq pt3 (cadr srcPts))
        (setq pt4 (cadr tarPts))

        ;; ------------------------------------------------------------
        ;; Calculate direction vectors
        ;; ------------------------------------------------------------

        (setq dx1 (- (car pt3) (car pt1)))
        (setq dy1 (- (cadr pt3) (cadr pt1)))

        (setq dx2 (- (car pt4) (car pt2)))
        (setq dy2 (- (cadr pt4) (cadr pt2)))

        ;; ------------------------------------------------------------
        ;; Ensure both point pairs have the same direction.
        ;; If they point opposite ways, reverse the target pair.
        ;; ------------------------------------------------------------

        (if (< (+ (* dx1 dx2) (* dy1 dy2)) 0.0)
          (progn
            (setq temp pt2)
            (setq pt2 pt4)
            (setq pt4 temp)
          )
        )

        ;; ------------------------------------------------------------
        ;; Calculate directions
        ;; ------------------------------------------------------------

        (setq ang1 (angle pt1 pt3))
        (setq ang2 (angle pt2 pt4))
                 
          (setq parallel
            (<
              (min
                (abs (- ang1 ang2))
                (abs (- (+ ang1 pi) ang2))
                (abs (- (- ang1 pi) ang2))
              )
              tol
            )
          )

          ;; ------------------------------------------------------------
          ;; If parallel, move source only perpendicular to target
          ;; ------------------------------------------------------------

          (if parallel
            (progn
              ;; --------------------------------------------------------
              ;; Perpendicular unit vector to target direction
              ;;
              ;; Target direction:
              ;;   (cos ang2, sin ang2)
              ;;
              ;; Perpendicular:
              ;;   (-sin ang2, cos ang2)
              ;; --------------------------------------------------------

              (setq perp
                (list
                  (sin ang2)
                  (- (cos ang2))
                  0.0
                )
              )

              ;; --------------------------------------------------------
              ;; Vector from source point to target point
              ;; --------------------------------------------------------

              (setq dx (- (car pt2) (car pt1)))
              (setq dy (- (cadr pt2) (cadr pt1)))

              ;; --------------------------------------------------------
              ;; Signed perpendicular distance
              ;; --------------------------------------------------------

              (setq dist
                (+
                  (* dx (car perp))
                  (* dy (cadr perp))
                )
              )

              ;; --------------------------------------------------------
              ;; Move source points perpendicular to the lines
              ;;
              ;; IMPORTANT:
              ;; X/Y position along the line is preserved.
              ;; Only the perpendicular component changes.
              ;; --------------------------------------------------------

              (setq pt2
                (list
                  (+ (car pt1) (* dist (car perp)))
                  (+ (cadr pt1) (* dist (cadr perp)))
                  (caddr pt1)
                )
              )

              (setq pt4
                (list
                  (+ (car pt3) (* dist (car perp)))
                  (+ (cadr pt3) (* dist (cadr perp)))
                  (caddr pt3)
                )
              )

              (princ
                "\nParallel: moving only perpendicular to the line."
              )
            )

            (princ
              "\nObjects are not parallel: normal ALIGN."
            )
          )

          ;; ------------------------------------------------------
          ;; ALIGN
          ;; ------------------------------------------------------

          (command
            "_.ALIGN"
            ss
            ""
            "_non" pt1
            "_non" pt2
            "_non" pt3
            "_non" pt4
            ""
            "_No"
          )
        )
      )
    )
  )
  (princ)
)

;;;;; segment extraction ;;;;;

(defun getSegmentPoints ( sel /  entName pick matrix obj objName bulge p1 p2 pointOnCurve param vindex seg)
  (setq entName (car sel)
        pick (cadr sel)
        matrix (caddr sel))
  (setq pick (trans pick 1 0))
  (if matrix
    (setq pick (MatrixInverseTransformPoint pick matrix))
  )
  
  (setq obj     (vlax-ename->vla-object entName)
        objName (vla-get-ObjectName obj)
        bulge   0.0)
  (cond
    ((= objName "AcDbLine")
      (setq p1 (vlax-curve-getstartpoint obj)
            p2 (vlax-curve-getendpoint obj)) 
    )
    ((= objName "AcDbXline")
      (setq ed (entget entName))
      (setq dir (cdr (assoc 11 ed)))
      (setq p1 (vlax-curve-getClosestPointTo obj pick))
      (setq p2 (mapcar '+ p1 dir)) ;'
    )
    ((= objName "AcDbPolyline")
      (setq seg (getSegFromPline obj pick matrix))
      (setq p1 (car seg)
            p2 (cadr seg)
            bulge (caddr seg))
    )
    ((= objName "AcDbHatch")
      (setq seg (getSegFromHatch obj pick matrix))
      (setq p1 (car seg)
            p2 (cadr seg)
            bulge (caddr seg))
    )
    ((= objName "AcDbWipeout")
      (setq seg (GetWipeoutSegment entName pick))
      (setq p1 (car seg)
            p2 (cadr seg))
    )
  )
  (if matrix
    (progn
      (setq p1 (MatrixTransformPoint p1 matrix))
      (setq p2 (MatrixTransformPoint p2 matrix))
    )
  )
  (list p1 p2 bulge)
)

(defun getSegFromPline (obj pick matrix / vindex param pointOnCurve bulge p1 p2)
  (setq pointOnCurve (vlax-curve-getclosestpointto obj pick))
  (setq param (vlax-curve-getParamAtPoint obj pointOnCurve))
  (setq vindex (fix param))
  (setq bulge (vla-GetBulge obj vindex))
  (if (and matrix (MatrixMirroredP matrix))
    (setq bulge (- bulge))
  )
  (setq p1 (vlax-curve-getPointAtParam obj vindex))
  (setq p2 (vlax-curve-getPointAtParam obj (1+ vindex)))
  (list p1 p2 bulge)   
)

(defun getSegFromHatch (hatchObj pick matrix / boundaries lastEnt bestDist bestBoundary obj pt dist hSeg)
  (setq lastEnt (entlast))
  (command "_HATCHGENERATEBOUNDARY" (vlax-vla-object->ename hatchObj) "")
  (while (setq lastEnt (entnext lastEnt))
    (setq boundaries (cons lastEnt boundaries))
  )
  (setq bestDist nil
        bestBoundary nil)
  (foreach boundary boundaries
    (setq obj (vlax-ename->vla-object boundary))
    (setq pt (vlax-curve-getClosestPointTo obj pick))
    (setq dist (distance pick pt))
    (if (or (null bestDist) (< dist bestDist))
      (setq bestDist dist
            bestBoundary boundary)
    )
  )
  (setq hSeg (getSegFromPline (vlax-ename->vla-object bestBoundary) pick matrix))
  (foreach boundary boundaries (entdel boundary))
  hSeg
)

(defun GetWipeoutSegment
  (ent pick / ed pts i p1 p2 cp d
       bestD bestP1 bestP2
       insPt uVec vVec)
  (setq ed (entget ent))
  (setq insPt (cdr (assoc 10 ed))
        uVec  (cdr (assoc 11 ed))
        vVec  (cdr (assoc 12 ed))
  )
  (setq pts
    (mapcar
      'cdr
      (vl-remove-if-not
        '(lambda (x)
           (= (car x) 14)
         )
        ed
      )
    )
  )
  (setq pts
    (reverse (cdr (reverse pts)))
  )
  (setq pts
    (mapcar
      '(lambda (pt)
         (mapcar '+
           insPt
           ;; U direction
           (mapcar
             '(lambda (u)
                (* (+ (car pt) 0.5) u)
              )
             uVec
           )
           ;; V direction
           ;; WIPEOUT Y axis is inverted
           (mapcar
             '(lambda (v)
                (* (- 0.5 (cadr pt)) v)
              )
             vVec
           )
         )
       )
      pts
    )
  )

  (setq i 0
        bestD nil)
  (repeat (length pts)
    (setq p1 (nth i pts))
    (setq p2
      (if (= i (1- (length pts)))
        (car pts)
        (nth (1+ i) pts)
      )
    )
    (setq cp
      (ClosestPointOnSegment pick p1 p2)
    )
    (setq d
      (distance pick cp)
    )
    (if (or (null bestD)
            (< d bestD))
      (setq bestD  d
            bestP1 p1
            bestP2 p2)
    )
    (setq i (1+ i))
  )
  (list bestP1 bestP2)
)

(defun ClosestPointOnSegment (p a b / ab ap u pt)
  (setq ab (mapcar '- b a)
        ap (mapcar '- p a))
  (setq u
    (if (> (apply '+ (mapcar '* ab ab)) 1e-12)
      (/ (apply '+ (mapcar '* ap ab))
         (apply '+ (mapcar '* ab ab)))
      0.0
    )
  )
  (setq u (max 0.0 (min 1.0 u)))
  (setq pt (mapcar
    '+
    a
    (mapcar
      '(lambda (x) (* x u))
      ab
    )
  ))
)

;;;;; polyline vertex manipulation ;;;;

(defun SetPolylineVertices
  (obj vindex1 pt1 pt2 / coords i1 i2 arr vindex2)
  (setq vindex2 (1+ vindex1))
  (setq coords
    (vlax-safearray->list
      (vlax-variant-value
        (vla-get-Coordinates obj)
      )
    )
  )

  (setq i1 (* 2 vindex1))
  (setq i2 (* 2 vindex2))

  (setq coords
    (subst (car pt1) (nth i1 coords) coords)
  )
  (setq coords
    (subst (cadr pt1) (nth (1+ i1) coords) coords)
  )
  (setq coords
    (subst (car pt2) (nth i2 coords) coords)
  )
  (setq coords
    (subst (cadr pt2) (nth (1+ i2) coords) coords)
  )

  (setq arr
    (vlax-make-safearray
      vlax-vbDouble
      (cons 0 (1- (length coords)))
    )
  )
  (vlax-safearray-fill arr coords)

  (vla-put-Coordinates obj arr)
)

;;;;;; matrix functions ;;;;;;

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