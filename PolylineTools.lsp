;;; DoubleVertex.lsp
;;; Command: DBLV
;;; Pick a segment of a lightweight polyline, then pick a point.
;;; Two coincident vertices are inserted at that point on the picked segment.
;;; Arc segments are split correctly (bulges are recalculated).

(vl-load-com)

;; Set (or add) a DXF group in an association list
(defun dblv:put (code val lst)
  (if (assoc code lst)
    (subst (cons code val) (assoc code lst) lst)
    (append lst (list (cons code val)))
  )
)

;; Centre of an arc segment from p1 to p2 with bulge b (OCS 2D points)
(defun dblv:center (p1 p2 b / th)
  (setq th (* 4.0 (atan (abs b))))
  (polar p1
         (+ (angle p1 p2) (* (if (minusp b) -1.0 1.0) (- (/ pi 2.0) (/ th 2.0))))
         (/ (distance p1 p2) (* 2.0 (sin (/ th 2.0))))
  )
)

(defun dblv:tan (a) (/ (sin a) (cos a)))

(defun c:DBVP (/ *error* doc undo sel ent ed n closed seg pt ptW par p p1 p2 b
                 theta cen del b1 b2 head chunks tail cur segc s e m frac newc i)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))

  (defun *error* (msg)
    (if undo (vla-EndUndoMark doc))
    (if (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*,*BREAK*"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  ;; --- Select the polyline segment ---
  (while (not ent)
    (setq sel (entsel "\nSelect polyline segment: "))
    (cond
      ((null sel)
       (if (= (getvar 'ERRNO) 7) (princ "\nMissed, try again.") (exit)))
      ((/= (cdr (assoc 0 (entget (car sel)))) "LWPOLYLINE")
       (princ "\nThat is not a (lightweight) polyline."))
      (t (setq ent (car sel)))
    )
  )

  (setq ed     (entget ent)
        n      (cdr (assoc 90 ed))
        closed (= 1 (logand 1 (cdr (assoc 70 ed))))
        seg    (fix (vlax-curve-getParamAtPoint ent
                      (vlax-curve-getClosestPointTo ent (trans (cadr sel) 1 0))))
        seg    (min seg (if closed (1- n) (- n 2)))
  )

  ;; --- Ask for the point, must lie on the picked segment ---
  (while (not ptW)
    (if (null (setq pt (getpoint "\nSpecify point on segment: "))) (exit))
    (setq ptW (vlax-curve-getClosestPointTo ent (trans pt 1 0))
          par (vlax-curve-getParamAtPoint ent ptW))
    (if (or (< par (- seg 1e-8)) (> par (+ seg 1 1e-8)))
      (progn (princ "\nPoint is not on the selected segment.") (setq ptW nil))
    )
  )

  ;; --- Split DXF data into header / vertex chunks / tail ---
  (foreach g ed
    (cond
      ((= (car g) 10)
       (if cur (setq chunks (cons (reverse cur) chunks)))
       (setq cur (list g)))
      ((and cur (member (car g) '(40 41 42 91))) (setq cur (cons g cur)))
      (cur (setq tail (cons g tail)))
      (t (setq head (cons g head)))
    )
  )
  (setq chunks (reverse (cons (reverse cur) chunks))
        head   (reverse head)
        tail   (reverse tail))

  ;; --- Geometry (OCS) ---
  (setq segc (nth seg chunks)
        p1   (cdr (assoc 10 segc))
        p2   (cdr (assoc 10 (nth (if (= (1+ seg) n) 0 (1+ seg)) chunks)))
        p    (trans ptW 0 ent)
        p    (list (car p) (cadr p))
        b    (cond ((cdr (assoc 42 segc))) (0.0))
        s    (cond ((cdr (assoc 40 segc))) (0.0))
        e    (cond ((cdr (assoc 41 segc))) (0.0))
        frac (max 0.0 (min 1.0 (- par seg)))
        m    (+ s (* (- e s) frac))
  )

  (if (equal b 0.0 1e-12)
    (setq b1 0.0 b2 0.0)
    (progn
      (setq theta (* 4.0 (atan b))
            cen   (dblv:center p1 p2 b))
      (cond
        ((equal p p1 1e-9) (setq del 0.0))
        ((equal p p2 1e-9) (setq del theta))
        (t
         (setq del (- (angle cen p) (angle cen p1)))
         (if (> b 0)
           (while (< del 0) (setq del (+ del (* 2 pi))))
           (while (> del 0) (setq del (- del (* 2 pi))))
         ))
      )
      (setq b1 (dblv:tan (/ del 4.0))
            b2 (dblv:tan (/ (- theta del) 4.0)))
    )
  )

  ;; --- Rebuild the vertex list with the two new vertices ---
  (setq i 0)
  (foreach c chunks
    (if (= i seg)
      (setq newc (append
                   (list (list (cons 10 p) (cons 40 m) (cons 41 e) (cons 42 b2))   ; 2nd new vertex -> p2
                         (list (cons 10 p) (cons 40 m) (cons 41 m) (cons 42 0.0))  ; 1st new vertex (zero-length seg)
                         (dblv:put 42 b1 (dblv:put 41 m c)))                       ; original start vertex
                   newc))
      (setq newc (cons c newc))
    )
    (setq i (1+ i))
  )
  (setq ed (append (dblv:put 90 (+ n 2) head)
                   (apply 'append (reverse newc))
                   tail))

  (vla-StartUndoMark doc)
  (setq undo t)
  (if (entmod ed)
    (progn (entupd ent) (princ "\nTwo coincident vertices inserted."))
    (princ "\nFailed to modify polyline.")
  )
  (vla-EndUndoMark doc)
  (princ)
)

;;; ------------------------------------------------------------------
;;; Commands: PLADD / PLREMOVE
;;; Select a closed lightweight polyline, then draw rectangles.
;;; PLADD merges each rectangle into the polyline boundary (union),
;;; PLREMOVE cuts each rectangle out of it (subtraction), like ArchiCAD's
;;; add / subtract polygon modes. Press Enter to finish.
;;; The original polyline keeps its handle and properties. If the result
;;; has holes or splits into several pieces, the extra loops are added as
;;; separate polylines with the same properties (the largest loop stays
;;; on the original polyline).
;;; ------------------------------------------------------------------

(setq *pladd:fuzz* 1e-6)

(defun pladd:dot (a b) (apply '+ (mapcar '* a b)))

;; WCS point -> 2D OCS point for normal nrm
(defun pladd:ocs (p nrm / q)
  (setq q (trans p 0 nrm))
  (list (car q) (cadr q))
)

;; Delete temporary objects still in the drawing
(defun pladd:cleanup ()
  (foreach o *pladd:tmp*
    (if (and o (not (vlax-erased-p o))) (vl-catch-all-apply 'vla-Delete (list o)))
  )
  (setq *pladd:tmp* nil)
)

;; Explode a region down to curves (nested regions are exploded too)
(defun pladd:explode (reg / out)
  (foreach o (vlax-invoke reg 'Explode)
    (setq *pladd:tmp* (cons o *pladd:tmp*))
    (if (= (vla-get-ObjectName o) "AcDbRegion")
      (setq out (append (pladd:explode o) out))
      (setq out (cons o out))
    )
  )
  (vla-Delete reg)
  out
)

;; Curves -> list of segments (p1 p2 bulge) in OCS, nil if unsupported curve
(defun pladd:segs (objs nrm / res bad c r b)
  (foreach o objs
    (cond
      ((= (vla-get-ObjectName o) "AcDbLine")
       (setq res (cons (list (pladd:ocs (vlax-get o 'StartPoint) nrm)
                             (pladd:ocs (vlax-get o 'EndPoint) nrm)
                             0.0)
                       res)))
      ((= (vla-get-ObjectName o) "AcDbArc")
       (setq b (dblv:tan (/ (vla-get-TotalAngle o) 4.0)))
       (if (minusp (pladd:dot (vlax-get o 'Normal) nrm)) (setq b (- b)))
       (setq res (cons (list (pladd:ocs (vlax-get o 'StartPoint) nrm)
                             (pladd:ocs (vlax-get o 'EndPoint) nrm)
                             b)
                       res)))
      ((= (vla-get-ObjectName o) "AcDbCircle")
       (setq c (pladd:ocs (vlax-get o 'Center) nrm)
             r (vla-get-Radius o))
       (setq res (append (list (list (list (+ (car c) r) (cadr c)) (list (- (car c) r) (cadr c)) 1.0)
                               (list (list (- (car c) r) (cadr c)) (list (+ (car c) r) (cadr c)) 1.0))
                         res)))
      (t (setq bad t))
    )
  )
  (if (not bad) res)
)

;; Reverse a segment
(defun pladd:rev (s) (list (cadr s) (car s) (- (caddr s))))

;; Find a segment in segs touching point pt; returns (original oriented)
(defun pladd:find (pt segs / found)
  (foreach s segs
    (cond
      (found)
      ((equal (car s) pt *pladd:fuzz*) (setq found (list s s)))
      ((equal (cadr s) pt *pladd:fuzz*) (setq found (list s (pladd:rev s))))
    )
  )
  found
)

;; Chain segments into closed loops; nil if any chain stays open
(defun pladd:chain (segs / loops loop cur start nxt bad)
  (while (and segs (not bad))
    (setq cur   (car segs)
          segs  (cdr segs)
          loop  (list cur)
          start (car cur))
    (while (and (not (equal (cadr cur) start *pladd:fuzz*))
                (setq nxt (pladd:find (cadr cur) segs)))
      (setq segs (vl-remove (car nxt) segs)
            cur  (cadr nxt)
            loop (cons cur loop))
    )
    (if (equal (cadr cur) start *pladd:fuzz*)
      (setq loops (cons (reverse loop) loops))
      (setq bad t)
    )
  )
  (if (not bad) loops)
)

;; Two straight segments that continue in the same direction
(defun pladd:collinear-p (a b / da db)
  (setq da (mapcar '- (cadr a) (car a))
        db (mapcar '- (cadr b) (car b)))
  (and (equal (caddr a) 0.0 1e-12)
       (equal (caddr b) 0.0 1e-12)
       (equal (- (* (car da) (cadr db)) (* (cadr da) (car db)))
              0.0
              (* 1e-9 (distance '(0 0) da) (distance '(0 0) db)))
       (> (pladd:dot da db) 0.0))
)

;; Remove zero-length segments and merge collinear straight segments
(defun pladd:simplify (loop / out)
  (foreach s loop
    (cond
      ((equal (car s) (cadr s) *pladd:fuzz*))
      ((and out (pladd:collinear-p (car out) s))
       (setq out (cons (list (car (car out)) (cadr s) 0.0) (cdr out))))
      (t (setq out (cons s out)))
    )
  )
  (setq out (reverse out))
  ;; wrap-around: last segment into first
  (if (and (cdr out) (pladd:collinear-p (last out) (car out)))
    (setq out (cons (list (car (last out)) (cadr (car out)) 0.0)
                    (cdr (reverse (cdr (reverse out)))))))
  out
)

;; Signed area of a loop (positive = counter-clockwise), arcs included
(defun pladd:area (loop / a p1 p2 b th r)
  (setq a 0.0)
  (foreach s loop
    (setq p1 (car s) p2 (cadr s) b (caddr s)
          a  (+ a (/ (- (* (car p1) (cadr p2)) (* (car p2) (cadr p1))) 2.0)))
    (if (not (equal b 0.0 1e-12))
      (setq th (* 4.0 (atan (abs b)))
            r  (/ (distance p1 p2) (* 2.0 (sin (/ th 2.0))))
            a  (+ a (* (if (minusp b) -1.0 1.0) 0.5 r r (- th (sin th))))))
  )
  a
)

;; Loop of the existing polyline (from its DXF data)
(defun pladd:pl-loop (ed / pts bl)
  (foreach g ed
    (cond
      ((= (car g) 10) (setq pts (cons (list (cadr g) (caddr g)) pts) bl (cons 0.0 bl)))
      ((and pts (= (car g) 42)) (setq bl (cons (cdr g) (cdr bl))))
    )
  )
  (setq pts (reverse pts) bl (reverse bl))
  (mapcar '(lambda (p q b) (list p q b)) pts (append (cdr pts) (list (car pts))) bl)
)

;; Replace the vertices of polyline ent with the loop
(defun pladd:apply (ent loop / ed head tail seen)
  (setq ed (entget ent))
  (foreach g ed
    (cond
      ((= (car g) 10) (setq seen t))
      ((member (car g) '(40 41 42 91)) (if (not seen) (setq head (cons g head))))
      (seen (setq tail (cons g tail)))
      (t (setq head (cons g head)))
    )
  )
  (setq head (reverse head)
        head (dblv:put 90 (length loop) head)
        head (dblv:put 70 (logior 1 (cdr (assoc 70 head))) head))
  (entmod (append head
                  (apply 'append
                         (mapcar '(lambda (s) (list (cons 10 (car s)) (cons 42 (caddr s)))) loop))
                  (reverse tail)))
  (entupd ent)
)

;; Add (mode acUnion) or remove (mode acSubtraction) the rectangle p1-p2
;; (UCS points) to/from closed polyline ent
(defun pladd:apply-rect (ent p1 p2 mode / doc obj ed nrm elev blk q rect robj res regA regB
                       inter aA aB aU loops outer holes sgn new)
  (setq doc  (vla-get-ActiveDocument (vlax-get-acad-object))
        obj  (vlax-ename->vla-object ent)
        ed   (entget ent)
        nrm  (cond ((cdr (assoc 210 ed))) ('(0.0 0.0 1.0)))
        elev (cond ((cdr (assoc 38 ed))) (0.0))
        blk  (vla-ObjectIDToObject doc (vla-get-OwnerID obj))
        q    (mapcar '(lambda (p) (setq p (trans p 1 ent)) (list (car p) (cadr p)))
                     (list p1
                           (list (car p2) (cadr p1) (caddr p1))
                           (list (car p2) (cadr p2) (caddr p1))
                           (list (car p1) (cadr p2) (caddr p1)))))
  (cond
    ((or (equal (car q) (cadr q) *pladd:fuzz*) (equal (cadr q) (caddr q) *pladd:fuzz*))
     (princ "\nRectangle has zero size."))
    ((not (setq rect (entmakex (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
                                             '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1)
                                             (cons 38 elev))
                                       (mapcar '(lambda (p) (cons 10 p)) q)
                                       (list (cons 210 nrm))))))
     (princ "\nCould not create the rectangle."))
    (t
     (setq robj        (vlax-ename->vla-object rect)
           *pladd:tmp* (list robj)
           res         (vl-catch-all-apply 'vlax-invoke (list blk 'AddRegion (list obj))))
     (cond
       ((or (vl-catch-all-error-p res) (/= (length res) 1))
        (princ "\nPolyline cannot be converted to a region (self-intersecting?)."))
       (t
        (setq regA        (car res)
              *pladd:tmp* (cons regA *pladd:tmp*)
              regB        (car (vlax-invoke blk 'AddRegion (list robj)))
              *pladd:tmp* (cons regB *pladd:tmp*)
              inter       (vlax-invoke obj 'IntersectWith robj acExtendNone)
              aA          (vla-get-Area regA)
              aB          (vla-get-Area regB))
        (vla-Boolean regA mode regB)
        (setq aU (vla-get-Area regA))
        (cond
          ((and (= mode acUnion) (null inter) (equal aU (+ aA aB) (* 1e-9 aU)))
           (princ "\nRectangle does not touch the polyline - not added."))
          ((and (= mode acUnion) (equal aU aA (* 1e-9 aU)))
           (princ "\nRectangle is already inside the polyline - nothing changed."))
          ((and (= mode acSubtraction) (equal aU 0.0 (* 1e-9 aA)))
           (princ "\nRectangle covers the whole polyline - not removed."))
          ((and (= mode acSubtraction) (equal aU aA (* 1e-9 aA)))
           (princ "\nRectangle does not overlap the polyline - nothing changed."))
          ((not (setq loops (pladd:segs (pladd:explode regA) nrm)
                      loops (pladd:chain loops)))
           (princ "\nCould not rebuild the boundary from the union result."))
          (t
           (setq loops (mapcar 'pladd:simplify loops)
                 outer (car loops))
           (foreach l (cdr loops)
             (if (> (abs (pladd:area l)) (abs (pladd:area outer))) (setq outer l))
           )
           (setq holes (vl-remove outer loops)
                 sgn   (minusp (pladd:area (pladd:pl-loop ed))))
           ;; keep the original winding direction
           (if (/= sgn (minusp (pladd:area outer)))
             (setq outer (reverse (mapcar 'pladd:rev outer))))
           (pladd:apply ent outer)
           (foreach h holes
             (setq new (vla-Copy obj))
             (pladd:apply (vlax-vla-object->ename new) h)
           )
           (princ (strcat (if (= mode acUnion) "\nRectangle added." "\nRectangle removed.")
                          (if holes (strcat " " (itoa (length holes)) " extra polyline(s) created.") "")))
          )
        )
       )
     )
     (pladd:cleanup)
    )
  )
)

;; Shared command loop for PLADD / PLREMOVE
(defun pladd:run (mode / *error* doc undo sel ent p1 p2)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        *pladd:tmp* nil)

  (defun *error* (msg)
    (pladd:cleanup)
    (if undo (vla-EndUndoMark doc))
    (if (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*,*BREAK*"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  ;; --- Use a preselected closed polyline if there is exactly one ---
  (if (and (setq sel (ssget "_I"))
           (= (sslength sel) 1)
           (= (cdr (assoc 0 (entget (ssname sel 0)))) "LWPOLYLINE")
           (= 1 (logand 1 (cdr (assoc 70 (entget (ssname sel 0)))))))
    (setq ent (ssname sel 0))
  )

  ;; --- Otherwise select a closed polyline ---
  (while (not ent)
    (setq sel (entsel "\nSelect closed polyline: "))
    (cond
      ((null sel)
       (if (= (getvar 'ERRNO) 7) (princ "\nMissed, try again.") (exit)))
      ((/= (cdr (assoc 0 (entget (car sel)))) "LWPOLYLINE")
       (princ "\nThat is not a (lightweight) polyline."))
      ((/= 1 (logand 1 (cdr (assoc 70 (entget (car sel))))))
       (princ "\nPolyline must be closed."))
      (t (setq ent (car sel)))
    )
  )

  (vla-StartUndoMark doc)
  (setq undo t)
  (sssetfirst nil (ssadd ent))
  ;; --- Draw rectangles until Enter ---
  (while (setq p1 (getpoint (if (= mode acUnion)
                                "\nFirst corner of rectangle to add <done>: "
                                "\nFirst corner of rectangle to remove <done>: ")))
    (if (setq p2 (getcorner p1 "\nOpposite corner: "))
      ;; re-select so the grips follow the new vertices
      (progn (pladd:apply-rect ent p1 p2 mode) (sssetfirst nil (ssadd ent)))
    )
  )
  (vla-EndUndoMark doc)
  (princ)
)

(defun c:PLADD () (pladd:run acUnion))
(defun c:PLREMOVE () (pladd:run acSubtraction))

(princ "\nPolylineTools loaded. Commands: DBVP (double vertex), PLADD / PLREMOVE (add / remove rectangle).")
(princ)
