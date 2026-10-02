(setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
(vl-load-com)

(setq joinedEnt nil)
(setq *SgCopyOffsetReactor* nil)
(setq tol (* 0.01 (/ pi 180.0)))

(defun c:SgCopy ( / sel p1 p2 selSet seg)
  (setq selSet (ssadd))
  (while (setq sel (nentselp "\nPick a segment : "))
    (setq seg   (getSegmentPoints sel)
          p1    (car seg)
          p2    (cadr seg)
          bulge (caddr seg)
          normal (cadddr seg))
    (if normal
      (progn
        (hs:make-pline seg)
        (princ "\ngot the hatch segment")
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
      (setq seg (HS:SegmentAtPick entName rawPick matrix))
      (setq p1 (car seg)
            p2 (cadr seg)
            bulge (caddr seg)
            normal (cadddr seg))
      (setq matrix nil)
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
  (list p1 p2 bulge normal)
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

  (if (setq sel (nentselp "\nPick a segment: "))
    (progn
      (setq seg (getSegmentPoints sel)
            p1  (trans (car seg) 0 1)
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

;; Polyline tipi loop vertex listesi ((pt bulge) ...) -> segmentler
(defun hs:verts->segs (vl / out)
  (while (cadr vl)
    (if (> (distance (caar vl) (caadr vl)) 1e-9)
      (setq out (cons (list (caar vl) (caadr vl) (cadar vl)) out))
    )
    (setq vl (cdr vl))
  )
  (reverse out)
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

(defun hs:hatch-segments (ed / lst segs flag closed verts items fix
                              typ p q c r a50 a51 ccw sw skipped)
  (setq lst     (cdr (member (assoc 91 ed) ed))
        skipped 0)
  (repeat (cdr (assoc 91 ed))
    (setq lst   (member (assoc 92 lst) lst)
          flag  (cdar lst)
          lst   (cdr lst)
          items nil)
    (if (= 2 (logand 2 flag))
      ;; --- Polyline tipi loop: 72 bulge var, 73 kapali, 93 adet, 10/42 ---
      (progn
        (setq closed (= 1 (cdr (assoc 73 lst)))
              lst    (cdr (member (assoc 93 lst) lst))
              verts  nil)
        (while (member (caar lst) '(10 42))
          (if (= 10 (caar lst))
            (setq verts (cons (list (hs:2d (cdar lst)) 0.0) verts))
            (setq verts (cons (list (car (car verts)) (cdar lst)) (cdr verts)))
          )
          (setq lst (cdr lst))
        )
        (setq verts (reverse verts))
        (if (and closed verts)
          (setq verts (append verts (list (list (car (car verts)) 0.0))))
        )
        (setq items (list (list 'F (hs:verts->segs verts))))
      )
      ;; --- Kenar tipi loop: 93 kenar adedi, her kenar 72 tip ile baslar ---
      (progn
        (repeat (cdr (assoc 93 lst))
          (setq lst (member (assoc 72 lst) lst)
                typ (cdar lst)
                lst (cdr lst))
          (cond
            ;; Cizgi
            ((= typ 1)
             (setq p (hs:2d (cdr (assoc 10 lst)))
                   q (hs:2d (cdr (assoc 11 lst))))
             (if (> (distance p q) 1e-9)
               (setq items (cons (list 'F (list (list p q 0.0))) items))
             )
            )
            ;; Daire yayi
            ((= typ 2)
             (setq c   (hs:2d (cdr (assoc 10 lst)))
                   r   (cdr (assoc 40 lst))
                   a50 (cdr (assoc 50 lst))
                   a51 (cdr (assoc 51 lst))
                   ccw (cdr (assoc 73 lst)))
             (if (/= 0 ccw)
               (setq items (cons (list 'F (hs:arc-segs c r a50 (hs:sweep a50 a51) 1)) items))
               ;; CW yay: acilar normalde x eksenine gore aynalanmis saklanir.
               ;; Emin olmak icin iki yorum da tutulur, komsu kenarlara gore secilir.
               (setq items
                 (cons (list 'A
                             (hs:arc-segs c r (- a50) (hs:sweep a50 a51) -1)  ; aynali
                             (hs:arc-segs c r a50 (hs:sweep a51 a50) -1))     ; dogrudan
                       items))
             )
            )
            ;; Elips (3) / spline (4)
            (T (setq skipped (1+ skipped)))
          )
        )
        (setq items (reverse items))
      )
    )
    ;; Belirsiz CW yaylari cozumle
    (setq fix nil)
    (foreach it items
      (if (eq (car it) 'F)
        (foreach s (cadr it) (setq fix (cons (car s) (cons (cadr s) fix))))
      )
    )
    (foreach it items
      (setq segs
        (append segs
          (cond
            ((eq (car it) 'F) (cadr it))
            ((< (hs:score (caddr it) fix) (- (hs:score (cadr it) fix) 1e-6)) (caddr it))
            (T (cadr it))
          )
        )
      )
    )
  )
  (if (> skipped 0)
    (princ (strcat "\nNot: " (itoa skipped) " adet elips/spline kenar atlandi."))
  )
  segs
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


(defun HS:SegmentAtPick (hent ppt mat / ed nrm elev segs xw yw nn q v pel
                                       s p1 p2 d bd best)
  (setq ed   (entget hent)
        nrm  (cdr (assoc 210 ed))
        elev (caddr (cdr (assoc 10 ed))))
  (if (null mat)
    (setq mat '((1.0 0.0 0.0 0.0) (0.0 1.0 0.0 0.0) (0.0 0.0 1.0 0.0) (0.0 0.0 0.0 1.0)))
  )
  (setq segs (hs:hatch-segments ed))
  ;; Donusmus duzlemin normali: donusmus OCS X ve Y eksenlerinin vektorel carpimi.
  ;; Aynalanmis bloklarda normal de doner, boylece bulge isareti degismeden kalir.
  (setq xw (hs:mxv mat (trans '(1.0 0.0 0.0) nrm 0 T))
        yw (hs:mxv mat (trans '(0.0 1.0 0.0) nrm 0 T))
        nn (hs:unit (hs:cross xw yw)))
  (if (and segs nn)
    (progn
      ;; Pick noktasini segment duzlemine bakis yonunde izdus
      (setq q (trans ppt 0 nn)
            v (trans (trans (getvar "VIEWDIR") 1 0 T) 0 nn T)
            pel (caddr (trans (hs:o2w (car (car segs)) nrm elev mat) 0 nn)))
      (if (> (abs (caddr v)) 1e-9)
        (setq q (mapcar '(lambda (a b) (+ a (* b (/ (- pel (caddr q)) (caddr v))))) q v))
      )
      (setq q (hs:2d q))
      ;; En yakin segment
      (foreach sg segs
        (setq p1 (hs:o2w (car sg) nrm elev mat)
              p2 (hs:o2w (cadr sg) nrm elev mat)
              s  (list (hs:2d (trans p1 0 nn)) (hs:2d (trans p2 0 nn)) (caddr sg))
              d  (hs:dist-seg q s))
        (if (or (null bd) (< d bd))
          (setq bd d best (list p1 p2 (caddr sg) nn))
        )
      )
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