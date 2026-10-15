(setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
(vl-load-com)

(setq joinedEnt nil)
(setq *SgCopyOffsetReactor* nil)
(setq tol (* 0.01 (/ pi 180.0)))

(defun c:SgCopy ( / sel p1 p2 selSet seg)
  (setq selSet (ssadd))
  (while (setq sel (nentselp "\nPick a segment : "))
    (if (setq seg (getSegmentPoints sel))
      (progn
        (setq p1    (car seg)
              p2    (cadr seg)
              bulge (caddr seg)
              normal (cadddr seg))
        (if normal
          (progn
            (hs:make-pline seg)
          )
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
        )
        (ssadd (entlast) selSet)
      )
      (princ "\nNo segment under the pickbox.")
    )
  )
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

;;;;; align ;;;;;

;; AlignOnSegment: source start/end -> target start/end (no scaling).
;; If the segments point opposite ways the target pair is swapped,
;; so the source never flips 180 degrees.
(defun c:AlOnSegment ( / data ss src tar s1 s2 t1 t2 tmp)
  (if (setq data (al:select))
    (progn
      (setq ss  (car data)
            src (cadr data)
            tar (caddr data)
            s1  (car src)
            s2  (cadr src)
            t1  (car tar)
            t2  (cadr tar))
      (if (< (hs:dot (al:dir2d s1 s2) (al:dir2d t1 t2)) 0.0)
        (setq tmp t1 t1 t2 t2 tmp)
      )
      (al:align ss s1 t1 s2 t2)
    )
  )
  (princ)
)

;; AlignToSegment:
;;   Parallel     -> source moves perpendicular to the segments onto the target line.
;;   Not parallel -> source is rotated onto the target line around its pick point;
;;                   the pick point lands on its perpendicular foot on the target line.
(defun c:TSegmentAlign ( / data ss src tar s1 s2 fit ps foot ds dt len)
  (if (setq data (al:select))
    (progn
      (setq ss   (car data)
            src  (cadr data)
            tar  (caddr data)
            s1   (car src)
            s2   (cadr src)
            fit  (al:fit s1 s2 (car tar) (cadr tar) (cadddr data))
            ps   (car fit)
            foot (cadr fit)
            ds   (caddr fit)
            dt   (cadddr fit)
            len  (distance (hs:2d s1) (hs:2d s2)))
      (al:align ss
                ps foot
                (mapcar '+ ps (al:scale ds len))
                (mapcar '+ foot (al:scale dt len)))
    )
  )
  (princ)
)

;; AlignSegment: edits the picked segment itself instead of moving its object.
;;   Source: LINE or LWPOLYLINE segment, not nested (it is modified).
;;   Target: any segment getSegmentPoints understands, nested or not.
;; Same rule as TSegmentAlign (parallel -> perpendicular move, otherwise rotate
;; around the pick point), segment length is kept. Neighbouring polyline
;; segments follow the moved vertices.
(defun c:ASegment ( / tar tarPts src srcPts obj typ fit ps foot ds dt
                       n1 n2 ename i j res)
  (cond
    ((null (setq tar (nentselp "\nSelect the segment to align to: "))))
    ((null (setq tarPts (getSegmentPoints tar)))
     (princ "\nNo segment under the pickbox."))
    ((< (distance (hs:2d (car tarPts)) (hs:2d (cadr tarPts))) 1e-9)
     (princ "\nTarget segment has zero length."))
    ((null (setq src (nentselp "\nSelect the segment to align: "))))
    ((cadddr src)
     (princ "\nSegment to align must not be nested."))
    ((not (member (setq ename (car src)
                        obj   (vlax-ename->vla-object ename)
                        typ   (vla-get-ObjectName obj))
                  '("AcDbLine" "AcDbPolyline")))
     (princ "\nSegment to align must be a line or a polyline segment."))
    ((null (setq srcPts (getSegmentPoints src)))
     (princ "\nNo segment under the pickbox."))
    ((< (distance (hs:2d (car srcPts)) (hs:2d (cadr srcPts))) 1e-9)
     (princ "\nSource segment has zero length."))
    (T
     (setq fit  (al:fit (car srcPts) (cadr srcPts) (car tarPts) (cadr tarPts)
                        (trans (cadr src) 1 0))
           ps   (car fit)
           foot (cadr fit)
           ds   (caddr fit)
           dt   (cadddr fit)
           n1   (al:rotate (car srcPts)  ps foot ds dt)
           n2   (al:rotate (cadr srcPts) ps foot ds dt))
     (vla-StartUndoMark doc)
     (setq res
       (vl-catch-all-apply
         '(lambda ()
            (if (= typ "AcDbLine")
              (progn
                (vla-put-StartPoint obj (vlax-3d-point n1))
                (vla-put-EndPoint   obj (vlax-3d-point n2))
              )
              (progn
                ;; Same segment index as getSegFromPline; end vertex wraps when closed
                (setq i (min (fix (vlax-curve-getParamAtPoint obj
                                    (vlax-curve-getClosestPointTo obj (trans (cadr src) 1 0))))
                             (1- (fix (vlax-curve-getEndParam obj))))
                      j (if (= (1+ i) (cdr (assoc 90 (entget ename)))) 0 (1+ i)))
                (al:put-vertex obj i (trans n1 0 ename))
                (al:put-vertex obj j (trans n2 0 ename))
              )
            )
            (vla-Update obj)
          )
       )
     )
     (vla-EndUndoMark doc)
     (if (vl-catch-all-error-p res)
       (princ (strcat "\nCould not modify the segment: "
                      (vl-catch-all-error-message res)))
     )
    )
  )
  (princ)
)

;; Shared fit for TSegmentAlign / AlSegment (all WCS).
;; Returns (ps foot ds dt): ps = pick point on the source line (source Z),
;; foot = its perpendicular foot on the target line, ds/dt = 2D unit directions.
;; When parallel dt = ds, so the result is a pure perpendicular move.
(defun al:fit (s1 s2 t1 t2 pick / ds dt ps foot)
  (setq ds (al:dir2d s1 s2)
        dt (al:dir2d t1 t2))
  (if (< (hs:dot ds dt) 0.0)
    (setq dt (mapcar '- dt))
  )
  ;; Pick point projected onto the source line (keeps the source Z)
  (setq ps (al:project pick s1 ds)
        ps (list (car ps) (cadr ps) (al:z s1)))
  ;; Its perpendicular foot on the target line, at the same Z
  (setq foot (al:project ps t1 dt)
        foot (list (car foot) (cadr foot) (caddr ps)))
  ;; Angle between the segments (radians)
  (if (< (abs (atan (- (* (car ds) (cadr dt)) (* (cadr ds) (car dt)))
                    (hs:dot ds dt)))
         1e-3)
    (progn
      (princ "\nParallel: moving perpendicular to the segments.")
      (setq dt ds)                    ; no rotation, pure perpendicular move
    )
    (princ "\nNot parallel: aligning around the pick point.")
  )
  (list ps foot ds dt)
)

;; Rigid 2D move of p: ps -> foot, direction ds -> dt. Keeps the Z of p.
(defun al:rotate (p ps foot ds dt / c s x y)
  (setq c (hs:dot ds dt)
        s (- (* (car ds) (cadr dt)) (* (cadr ds) (car dt)))
        x (- (car p) (car ps))
        y (- (cadr p) (cadr ps)))
  (list (+ (car foot) (- (* c x) (* s y)))
        (+ (cadr foot) (* s x) (* c y))
        (al:z p))
)

;; LWPOLYLINE vertex i <- OCS point p (2D)
(defun al:put-vertex (obj i p / sa)
  (setq sa (vlax-make-safearray vlax-vbDouble '(0 . 1)))
  (vlax-safearray-fill sa (list (car p) (cadr p)))
  (vla-put-Coordinate obj i (vlax-make-variant sa))
)

;; Prompts for target and source segments.
;; Returns (ss srcPts tarPts srcPickWCS) or nil
(defun al:select ( / tar src parents ss srcPts tarPts)
  (cond
    ((null (setq tar (nentselp "\nSelect the segment to align to: "))))
    ((null (setq tarPts (getSegmentPoints tar)))
     (princ "\nNo segment under the pickbox."))
    ((null (setq src (nentselp "\nSelect the segment to align: "))))
    ((null (setq srcPts (getSegmentPoints src)))
     (princ "\nNo segment under the pickbox."))
    ((< (distance (hs:2d (car srcPts)) (hs:2d (cadr srcPts))) 1e-9)
     (princ "\nSource segment has zero length."))
    ((< (distance (hs:2d (car tarPts)) (hs:2d (cadr tarPts))) 1e-9)
     (princ "\nTarget segment has zero length."))
    (T
     ;; Move the outermost owner (block reference) when the pick was nested
     (setq parents (cadddr src)
           ss      (ssadd))
     (ssadd (cond ((and parents (listp parents)) (last parents))
                  (parents)
                  ((car src)))
            ss)
     (list ss srcPts tarPts (trans (cadr src) 1 0))
    )
  )
)

;; WCS points -> UCS, then ALIGN without scaling
(defun al:align (ss a1 b1 a2 b2)
  (command
    "_.ALIGN" ss ""
    "_non" (trans a1 0 1)
    "_non" (trans b1 0 1)
    "_non" (trans a2 0 1)
    "_non" (trans b2 0 1)
    ""
    "_No"
  )
)

;; 2D unit direction a -> b as (x y 0.0)
(defun al:dir2d (a b)
  (hs:unit (list (- (car b) (car a)) (- (cadr b) (cadr a)) 0.0))
)

(defun al:scale (v k) (mapcar '(lambda (x) (* x k)) v))

(defun al:z (p) (cond ((caddr p)) (0.0)))

;; 2D projection of p onto the line through a with unit direction d
(defun al:project (p a d / k)
  (setq k (+ (* (- (car p) (car a)) (car d))
             (* (- (cadr p) (cadr a)) (cadr d))))
  (list (+ (car a) (* k (car d))) (+ (cadr a) (* k (cadr d))) 0.0)
)

;;;;; segment extraction ;;;;;

(defun getSegmentPoints ( sel / entName rawPick pick matrix obj objName bulge p1 p2
          seg ed dir tmpHatch normal)
  (setq entName (car sel)
        rawPick (cadr sel)
        matrix (caddr sel))
  (setq rawPick (trans rawPick 1 0))
  (if matrix
    (setq pick (MatrixInverseTransformPoint rawPick matrix))
    (setq pick rawPick)
  )
  (setq obj     (vlax-ename->vla-object entName)
        objName (vla-get-ObjectName obj)
        bulge   0.0)
  (cond
    ((= objName "AcDbLine")
      (princ "\nLine segment")
      (setq p1 (vlax-curve-getstartpoint obj)
            p2 (vlax-curve-getendpoint obj)) 
    )
    ((= objName "AcDbXline")
      (princ "\nXline segment")
      (setq ed (entget entName))
      (setq dir (cdr (assoc 11 ed)))
      (setq p1 (vlax-curve-getClosestPointTo obj pick))
      (setq p2 (mapcar '+ p1 dir)) ;'
    )
    ((= objName "AcDbPolyline")
      (princ "\nPolyline segment")
      (setq seg (getSegFromPline obj pick matrix))
      (setq p1 (car seg)
            p2 (cadr seg)
            bulge (caddr seg))
    )
    ((= objName "AcDbHatch")
      (princ "\nHatch segment")
      (setq seg (HS:SegmentAtPick entName rawPick matrix))
      (setq p1 (car seg)
            p2 (cadr seg)
            bulge (caddr seg)
            normal (cadddr seg))
      (setq matrix nil)
    )
    ((= objName "AcDbWipeout")
      (princ "\nWipeout segment")
      (setq seg (GetWipeoutSegment entName pick))
      (setq p1 (car seg)
            p2 (cadr seg))
    )
    ((= objName "AcDbMline")
      (princ "\nMultiline segment")
      (setq seg (GetMlineSegment entName pick))
      (setq p1 (car seg)
            p2 (cadr seg))
    )
  )
  (if (and p1 p2)
    (progn
      (if matrix
        (progn
          (setq p1 (MatrixTransformPoint p1 matrix))
          (setq p2 (MatrixTransformPoint p2 matrix))
        )
      )
      (list p1 p2 bulge normal)
    )
  )
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

(defun GetWipeoutSegment
  (ent pick / ed pts i p1 p2 cp d
             bestD bestP1 bestP2 insPt uVec vVec)
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

;; MLINE: every element line is a separate candidate.
;; Element point at a vertex = vertex + miter * (first 41 param of that element)
(defun GetMlineSegment
  (ent pick / ed closed lst x v m first cur verts n i j e
             p1 p2 cp d bestD bestP1 bestP2)
  (setq ed     (entget ent)
        closed (= 2 (logand 2 (cdr (assoc 71 ed))))
        lst    (member (assoc 11 ed) ed))
  ;; verts: ((elem0Pt elem1Pt ...) ...) one entry per vertex
  (foreach x lst
    (cond
      ((= (car x) 11)
       (if v (setq verts (cons (reverse cur) verts)))
       (setq v (cdr x) cur nil))
      ((= (car x) 13)
       (setq m (cdr x)))
      ((= (car x) 74)
       (if (= (cdr x) 0)
         (setq cur (cons v cur) first nil)
         (setq first T)))
      ((and (= (car x) 41) first)
       (setq cur   (cons (mapcar '(lambda (a b) (+ a (* b (cdr x)))) v m) cur)
             first nil))
    )
  )
  (if v (setq verts (cons (reverse cur) verts)))
  (setq verts (reverse verts)
        n     (length verts)
        i     0)
  (repeat (if closed n (1- n))
    (setq j (if (= i (1- n)) 0 (1+ i))
          e 0)
    (repeat (length (nth i verts))
      (setq p1 (nth e (nth i verts))
            p2 (nth e (nth j verts)))
      (if (and p1 p2 (> (distance p1 p2) 1e-9))
        (progn
          (setq cp (ClosestPointOnSegment pick p1 p2)
                d  (distance pick cp))
          (if (or (null bestD) (< d bestD))
            (setq bestD  d
                  bestP1 p1
                  bestP2 p2)
          )
        )
      )
      (setq e (1+ e))
    )
    (setq i (1+ i))
  )
  (if bestP1 (list bestP1 bestP2))
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


;;;;;; cmd reactor ;;;;;;

(defun c:OffsetJoined ( / ent ss )
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
          '((:vlr-commandEnded . SgCopy-OffsetEnded)) ;'
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

(defun SgCopy-OffsetEnded (reactor params / cmd)
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

;;;;;; XLINE on segment ;;;;;;

(defun XLOnSegment ( / sel seg p1 p2 ang )

  (if (and (setq sel (nentselp "\nPick a segment: "))
           (setq seg (getSegmentPoints sel)))
    (progn
      (setq p1  (trans (car seg) 0 1)
            p2  (trans (cadr seg) 0 1))
      (setq ang (angle p1 p2))
      
      ;; Normalize angle to -90 ... +90
      (if (> ang (/ pi 2.0))
        (setq ang (- ang pi))
      )
      
      ;; Snap nearly horizontal / vertical
      (cond
        ;; Near 0 degrees
        ((< (abs ang) tol)
         (setq ang 0.0)
        )

        ;; Near 90 degrees
        ((< (abs (- (abs ang) (/ pi 2.0))) tol)
         (setq ang
           (if (< ang 0.0)
             (- (/ pi 2.0))
             (/ pi 2.0)
           )
         )
        )
      )

      ;; Create XLINE exactly at p1
      (command
        "_.XLINE"
        "_A"
        (angtos ang 0 8)
        p1
        ""
      )

      ;; Offset the newly created XLINE
      (c:OffsetJoined)
    )
    (progn
      (command "_.XLINE")
    )
  )
  (princ)
)

(defun c:CX ( )
  (XLOnSegment)
  (princ)
)



;;;;; ******************  HSEG  ********************** ;;;;;;;

;;; ------------------------------------------------------------------
;;; Vektor / matris yardimcilari
;;; ------------------------------------------------------------------

(defun hs:dot (u v) (apply '+ (mapcar '* u v)))
(defun hs:len (v) (sqrt (hs:dot v v)))
(defun hs:2d (p) (list (car p) (cadr p)))
(defun hs:tan (a) (/ (sin a) (cos a)))

(defun hs:unit (v / l)
  (setq l (hs:len v))
  (if (> l 1e-12) (mapcar '(lambda (x) (/ x l)) v))
)

(defun hs:cross (u v)
  (list (- (* (cadr u) (caddr v)) (* (caddr u) (cadr v)))
        (- (* (caddr u) (car v)) (* (car u) (caddr v)))
        (- (* (car u) (cadr v)) (* (cadr u) (car v))))
)

;; 0 <= a < 2pi
(defun hs:nrm (a)
  (setq a (rem a (+ pi pi)))
  (if (< a 0.0) (+ a pi pi) a)
)

;; 4x4 matris * nokta (MCS -> WCS)
(defun hs:mxp (m p)
  (mapcar '(lambda (r) (+ (hs:dot (list (car r) (cadr r) (caddr r)) p) (cadddr r)))
          (list (car m) (cadr m) (caddr m)))
)

;; 4x4 matrisin 3x3 kismi * vektor
(defun hs:mxv (m v)
  (mapcar '(lambda (r) (hs:dot (list (car r) (cadr r) (caddr r)) v))
          (list (car m) (cadr m) (caddr m)))
)

;; Hatch OCS 2D nokta -> WCS 3D
(defun hs:o2w (p nrm elev mat)
  (hs:mxp mat (trans (list (car p) (cadr p) elev) nrm 0))
)

;;; ------------------------------------------------------------------
;;; Bulge geometrisi ve 2D mesafe
;;; ------------------------------------------------------------------

;; (merkez yaricap)
(defun hs:bulge-center (p1 p2 b / th r)
  (setq th (* 4.0 (atan b))
        r  (/ (distance p1 p2) (* 2.0 (sin (/ th 2.0)))))
  (list (polar p1 (+ (angle p1 p2) (- (/ pi 2.0) (/ th 2.0))) r) (abs r))
)

(defun hs:dist-line (q a b / ab d2 k)
  (setq ab (mapcar '- b a)
        d2 (hs:dot ab ab))
  (if (< d2 1e-18)
    (distance q a)
    (progn
      (setq k (max 0.0 (min 1.0 (/ (hs:dot (mapcar '- q a) ab) d2))))
      (distance q (mapcar '(lambda (s d) (+ s (* d k))) a ab))
    )
  )
)

(defun hs:dist-arc (q p1 p2 b / cr c r a1 a2)
  (setq cr (hs:bulge-center p1 p2 b)
        c  (car cr)
        r  (cadr cr))
  ;; yayi her zaman CCW yonde a1 -> a2 olarak ele al
  (if (> b 0.0)
    (setq a1 (angle c p1) a2 (angle c p2))
    (setq a1 (angle c p2) a2 (angle c p1))
  )
  (if (<= (hs:nrm (- (angle c q) a1)) (+ (hs:nrm (- a2 a1)) 1e-12))
    (abs (- (distance c q) r))
    (min (distance q p1) (distance q p2))
  )
)

;; seg = (p1 p2 bulge), 2D
(defun hs:dist-seg (q seg)
  (if (< (abs (caddr seg)) 1e-12)
    (hs:dist-line q (car seg) (cadr seg))
    (hs:dist-arc q (car seg) (cadr seg) (caddr seg))
  )
)

;;; ------------------------------------------------------------------
;;; Hatch boundary verisini segment listesine cevirme (hatch OCS, 2D)
;;; ------------------------------------------------------------------

;; from -> to arasindaki CCW aci farki, 0 ise tam daire
(defun hs:sweep (from to / s)
  (setq s (hs:nrm (- to from)))
  (if (< s 1e-9) (+ pi pi) s)
)

;; Yay -> segment listesi. dir: 1 = CCW, -1 = CW
(defun hs:arc-segs (c r sa sw dir / pa pm)
  (setq pa (polar c sa r))
  (if (>= sw (- (+ pi pi) 1e-9))
    (progn
      (setq pm (polar c (+ sa (* dir pi)) r))
      (list (list pa pm (float dir)) (list pm pa (float dir)))
    )
    (list (list pa (polar c (+ sa (* dir sw)) r) (* dir (hs:tan (/ sw 4.0)))))
  )
)

(defun hs:mind (p pts)
  (apply 'min (mapcar '(lambda (x) (distance p x)) pts))
)

;; Aday segment zincirinin uclarinin, loop'taki kesin uc noktalara uzakligi
(defun hs:score (sl pts)
  (if pts
    (+ (hs:mind (car (car sl)) pts) (hs:mind (cadr (last sl)) pts))
    0.0
  )
)

;;; ------------------------------------------------------------------
;;; Hatch boundary'yi okurken her segmenti aninda pickbox'a gore test etme
;;; ------------------------------------------------------------------

;; sl: hatch OCS segment listesi ((p1 p2 bulge) ...)
;; ctx: (nrm elev mat nn q pb)
;; Pickbox icindeki ilk segmenti (p1w p2w bulge nn) olarak dondurur, yoksa nil
(defun hs:seg-hit (sl ctx / nrm elev mat nn q pb sg p1 p2 hit)
  (setq nrm  (nth 0 ctx)
        elev (nth 1 ctx)
        mat  (nth 2 ctx)
        nn   (nth 3 ctx)
        q    (nth 4 ctx)
        pb   (nth 5 ctx))
  (while (and sl (null hit))
    (setq sg (car sl)
          sl (cdr sl))
    (if (> (distance (car sg) (cadr sg)) 1e-9)
      (progn
        (setq p1 (hs:o2w (car sg) nrm elev mat)
              p2 (hs:o2w (cadr sg) nrm elev mat))
        (if (<= (hs:dist-seg q (list (hs:2d (trans p1 0 nn)) (hs:2d (trans p2 0 nn)) (caddr sg)))
                pb)
          (setq hit (list p1 p2 (caddr sg) nn))
        )
      )
    )
  )
  hit
)

;; Segment listesinin uc noktalarini fix listesine ekle
(defun hs:add-fix (sl fix)
  (foreach s sl (setq fix (cons (car s) (cons (cadr s) fix))))
  fix
)

;; Boundary loop'larini sirayla okur, pickbox icindeki ilk segmentte durur
(defun hs:hatch-hit (ed ctx / lst n k flag closed first prev p q typ sl
                             c r a50 a51 fix amb skipped best)
  (setq lst     (cdr (member (assoc 91 ed) ed))
        n       (cdr (assoc 91 ed))
        skipped 0)
  (while (and (> n 0) (null best))
    (setq n    (1- n)
          lst  (member (assoc 92 lst) lst)
          flag (cdar lst)
          lst  (cdr lst))
    (if (= 2 (logand 2 flag))
      ;; --- Polyline tipi loop: 72 bulge var, 73 kapali, 93 adet, 10/42 ---
      (progn
        (setq closed (= 1 (cdr (assoc 73 lst)))
              lst    (cdr (member (assoc 93 lst) lst))
              first  nil
              prev   nil)
        (while (and (member (caar lst) '(10 42)) (null best))
          (if (= 10 (caar lst))
            (progn
              (setq p (hs:2d (cdar lst)))
              (if prev
                (setq best (hs:seg-hit (list (list (car prev) p (cadr prev))) ctx))
                (setq first p)
              )
              (setq prev (list p 0.0))
            )
            ;; 42: onceki vertex'in bulge'u
            (setq prev (list (car prev) (cdar lst)))
          )
          (setq lst (cdr lst))
        )
        ;; Kapanis segmenti
        (if (and closed prev (null best))
          (setq best (hs:seg-hit (list (list (car prev) first (cadr prev))) ctx))
        )
      )
      ;; --- Kenar tipi loop: 93 kenar adedi, her kenar 72 tip ile baslar ---
      (progn
        (setq k   (cdr (assoc 93 lst))
              fix nil
              amb nil)
        (while (and (> k 0) (null best))
          (setq k   (1- k)
                lst (member (assoc 72 lst) lst)
                typ (cdar lst)
                lst (cdr lst)
                sl  nil)
          (cond
            ;; Cizgi
            ((= typ 1)
             (setq p  (hs:2d (cdr (assoc 10 lst)))
                   q  (hs:2d (cdr (assoc 11 lst)))
                   sl (list (list p q 0.0)))
            )
            ;; Daire yayi
            ((= typ 2)
             (setq c   (hs:2d (cdr (assoc 10 lst)))
                   r   (cdr (assoc 40 lst))
                   a50 (cdr (assoc 50 lst))
                   a51 (cdr (assoc 51 lst)))
             (if (/= 0 (cdr (assoc 73 lst)))
               (setq sl (hs:arc-segs c r a50 (hs:sweep a50 a51) 1))
               ;; CW yay: acilar normalde x eksenine gore aynalanmis saklanir.
               ;; Iki yorum da tutulur, loop sonunda komsu kenarlara gore secilip test edilir.
               (setq amb
                 (cons (list (hs:arc-segs c r (- a50) (hs:sweep a50 a51) -1)  ; aynali
                             (hs:arc-segs c r a50 (hs:sweep a51 a50) -1))     ; dogrudan
                       amb))
             )
            )
            ;; Elips (3) / spline (4)
            (T (setq skipped (1+ skipped)))
          )
          (if sl
            (setq fix  (hs:add-fix sl fix)
                  best (hs:seg-hit sl ctx))
          )
        )
        ;; Belirsiz CW yaylari cozumle ve test et
        (foreach a (reverse amb)
          (if (null best)
            (setq best
              (hs:seg-hit
                (if (< (hs:score (cadr a) fix) (- (hs:score (car a) fix) 1e-6))
                  (cadr a)
                  (car a)
                )
                ctx
              )
            )
          )
        )
      )
    )
  )
  (if (and (null best) (> skipped 0))
    (princ (strcat "\nNot: " (itoa skipped) " adet elips/spline kenar atlandi."))
  )
  best
)

;; Bulge of the arc through 2D points a -> m -> c
(defun hs:bulge-3p (a m c / cr al v1 v2)
  (setq cr (- (* (- (car c) (car a)) (- (cadr m) (cadr a)))
              (* (- (cadr c) (cadr a)) (- (car m) (car a)))))
  (if (< (abs cr) 1e-9)
    0.0                                   ; collinear -> straight segment
    (progn
      (setq v1 (mapcar '- a m)
            v2 (mapcar '- c m)
            al (abs (- (angle '(0 0) v1) (angle '(0 0) v2))))
      (if (> al pi) (setq al (- (+ pi pi) al)))   ; interior angle at m
      (* (if (< cr 0.0) 1.0 -1.0) (/ 1.0 (hs:tan (/ al 2.0))))
    )
  )
)

(defun hs:make-pline (seg / p1 p2 b nn o1 o2 om f1 f2 fm)
  (setq p1 (car seg)
        p2 (cadr seg)
        b  (caddr seg)
        nn (cadddr seg)
        f1 (hs:2d p1)
        f2 (hs:2d p2))
  ;; Midpoint of the original arc, worked out in its own plane, then flattened
  (if (/= b 0.0)
    (progn
      (setq o1 (trans p1 0 nn)
            o2 (trans p2 0 nn)
            om (list (+ (/ (+ (car o1) (car o2)) 2.0)
                        (* (/ b 2.0) (- (cadr o2) (cadr o1))))
                     (- (/ (+ (cadr o1) (cadr o2)) 2.0)
                        (* (/ b 2.0) (- (car o2) (car o1))))
                     (caddr o1))
            fm (hs:2d (trans om nn 0))
            b  (hs:bulge-3p f1 fm f2))
    )
  )
  (if (> (distance f1 f2) 1e-9)
    (entmakex
      (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")
            '(90 . 2) '(70 . 0) '(38 . 0.0)
            (list 10 (car f1) (cadr f1))
            (cons 42 b)
            (list 10 (car f2) (cadr f2))
            '(42 . 0.0)
            '(210 0.0 0.0 1.0))
    )
  )
)


;; Pickbox yari genisligi, cizim birimi cinsinden (aktif viewport)
(defun hs:pickbox-size ()
  (* (getvar "PICKBOX")
     (/ (getvar "VIEWSIZE") (cadr (getvar "SCREENSIZE"))))
)

(defun HS:SegmentAtPick (hent ppt mat / ed nrm elev xw yw nn q v pel best)
  (setq ed   (entget hent)
        nrm  (cdr (assoc 210 ed))
        elev (caddr (cdr (assoc 10 ed))))
  (if (null mat)
    (setq mat '((1.0 0.0 0.0 0.0) (0.0 1.0 0.0 0.0) (0.0 0.0 1.0 0.0) (0.0 0.0 0.0 1.0)))
  )
  ;; Donusmus duzlemin normali: donusmus OCS X ve Y eksenlerinin vektorel carpimi.
  ;; Aynalanmis bloklarda normal de doner, boylece bulge isareti degismeden kalir.
  (setq xw (hs:mxv mat (trans '(1.0 0.0 0.0) nrm 0 T))
        yw (hs:mxv mat (trans '(0.0 1.0 0.0) nrm 0 T))
        nn (hs:unit (hs:cross xw yw)))
  (if nn
    (progn
      ;; Pick noktasini segment duzlemine bakis yonunde izdus
      (setq q (trans ppt 0 nn)
            v (trans (trans (getvar "VIEWDIR") 1 0 T) 0 nn T)
            pel (caddr (trans (hs:o2w '(0.0 0.0) nrm elev mat) 0 nn)))
      (if (> (abs (caddr v)) 1e-9)
        (setq q (mapcar '(lambda (a b) (+ a (* b (/ (- pel (caddr q)) (caddr v))))) q v))
      )
      (setq q (hs:2d q))
      ;; Boundary okunurken pickbox icindeki ilk segmentte durulur
      (setq best (hs:hatch-hit ed (list nrm elev mat nn q (hs:pickbox-size))))
      (if (and best
               (/= 0.0 (caddr best))
               (or (> (abs (- (hs:len xw) (hs:len yw))) (* 1e-6 (hs:len xw)))
                   (> (abs (hs:dot (hs:unit xw) (hs:unit yw))) 1e-6)))
        (princ "\nUyari: blok olcegi esit degil, yay WCS'de elips olur; kopya yaklasiktir.")
      )
      best
    )
  )
)