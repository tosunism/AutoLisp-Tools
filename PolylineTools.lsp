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

(defun c:DBLV (/ *error* doc undo sel ent ed n closed seg pt ptW par p p1 p2 b
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

(princ "\nDoubleVertex loaded. Type DBLV to run.")
(princ)
