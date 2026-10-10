;;; ===================================================================
;;; FLATTEN-Z.LSP
;;;
;;; Recursively zeroes Z coordinates (and elevations) on a selection
;;; set, descending into blocks at any nesting depth.
;;;
;;; Commands: FLATTENZ    - flatten the selection
;;;           FLATTENZALL - flatten every object in the drawing
;;;           FLZCHECK    - diagnose one (possibly nested) object's real Z
;;;
;;; Logic:
;;;   - Not a block  -> zero every 3D point group code on the entity
;;;                      (start/end points, centers, elevation, etc.)
;;;   - Is a block    -> zero the INSERT's own insertion point, then
;;;     (INSERT)        flatten its attached attributes, then recurse
;;;                      into the block DEFINITION and do the same to
;;;                      everything inside it (including nested INSERTs,
;;;                      arbitrarily deep), and zero the definition's
;;;                      base point Z.
;;;
;;; Implementation notes:
;;;   - Uses classic entget/entmod/entnext (DXF group codes); ActiveX is
;;;     only used for the block definition base point.
;;;   - Generic point-zeroing: any group code whose value is a 3-element
;;;     real list (a WCS/OCS point) gets its Z set to 0.0. Group 38
;;;     (elevation) and 39 (thickness) are zeroed explicitly.
;;;   - Extrusion normal (group 210): a normal that is *almost* +Z or -Z
;;;     (e.g. (1e-12 -3e-11 1.0)) is the usual cause of "invisible" Z:
;;;     the entity's OCS is very slightly tilted, so although its OCS
;;;     elevation is 0, its real WCS vertices get Z = tilt * distance
;;;     from origin. Such normals are snapped to exactly (0 0 +/-1),
;;;     and for OCS-based entities the points are first transformed
;;;     into the new OCS so nothing moves in XY. The sign is kept, so
;;;     mirrored (0 0 -1) geometry stays valid. Normals tilted by more
;;;     than *flz-normal-tol* are left alone and counted in the report.
;;;   - Block DEFINITIONS are only processed once each (tracked in
;;;     *flz-done-blocks*), since a definition is shared by every
;;;     INSERT that references it. This also prevents infinite
;;;     recursion on any pathological/self-referencing block.
;;;   - XREF-bound blocks (bit 4 of group 70) are skipped at the
;;;     definition level -- their geometry lives in an external
;;;     drawing, only the xref INSERT's own insertion point is
;;;     flattened.
;;;   - Old-style POLYLINE/VERTEX/SEQEND chains and INSERT/ATTRIB/
;;;     SEQEND chains are walked explicitly via entnext, since those
;;;     sub-entities aren't reachable through entget alone.
;;; ===================================================================

(vl-load-com)

;; normals with |X| and |Y| below this are treated as "meant to be Z"
(setq *flz-normal-tol* 1e-6)

;; entity types whose point group codes are stored in OCS
(setq *flz-ocs-types*
  '("ARC" "CIRCLE" "LWPOLYLINE" "POLYLINE" "TEXT" "ATTRIB" "ATTDEF"
    "INSERT" "SHAPE" "SOLID" "TRACE"))

;; ---- exact (0 0 +/-1) if n is nearly +/-Z, else nil ----
(defun flz-snap-normal (n)
  (if (and n
           (< (abs (car n)) *flz-normal-tol*)
           (< (abs (cadr n)) *flz-normal-tol*))
    (list 0.0 0.0 (if (minusp (caddr n)) -1.0 1.0))))

(defun flz-3d-point-p (val)
  (and (listp val) (= (length val) 3)
       (numberp (car val)) (numberp (cadr val)) (numberp (caddr val))))

(defun flz-2d-point-p (val)
  (and (listp val) (= (length val) 2)
       (numberp (car val)) (numberp (cadr val))))

;; ---- zero every point Z, elevation and thickness; fix near-Z normals ----
;; ocsn: OCS normal for entities that don't carry their own 210
;;       (VERTEX of a 2D POLYLINE gets its parent's normal), else nil
(defun flz-zero-point-and-elev (elist ocsn / etype n tn ocs elev newlist item code val p)
  (setq etype (cdr (assoc 0 elist))
        n     (cond (ocsn) ((cdr (assoc 210 elist))) ('(0.0 0.0 1.0)))
        tn    (flz-snap-normal n)
        ocs   (or ocsn (member etype *flz-ocs-types*))
        elev  (cond ((cdr (assoc 38 elist))) (0.0))
        newlist nil)
  (if (and (null tn) (assoc 210 elist))
    (setq *flz-tilted* (1+ *flz-tilted*)))
  (foreach item elist
    (setq code (car item)
          val  (cdr item))
    (cond
      ((= code 210)
       (setq newlist (cons (cons 210 (if tn tn val)) newlist)))
      ((flz-3d-point-p val)
       (setq p (if (and tn ocs) (trans val n tn) val))
       (setq newlist (cons (list code (car p) (cadr p) 0.0) newlist)))
      ;; LWPOLYLINE vertices are 2D OCS points at the entity elevation
      ((and tn (= etype "LWPOLYLINE") (= code 10) (flz-2d-point-p val))
       (setq p (trans (list (car val) (cadr val) elev) n tn))
       (setq newlist (cons (list code (car p) (cadr p)) newlist)))
      ((or (= code 38) (= code 39))
       (setq newlist (cons (cons code 0.0) newlist)))
      (t
       (setq newlist (cons item newlist)))))
  (reverse newlist))

;; ---- entmod wrapped defensively so one bad entity doesn't abort the run ----
(defun flz-safe-entmod (elist / res)
  (setq res (vl-catch-all-apply 'entmod (list elist)))
  (if (vl-catch-all-error-p res)
    (prompt (strcat "\nFLATTENZ: could not modify entity (handle "
                     (cdr (assoc 5 elist)) "): "
                     (vl-catch-all-error-message res)))))

;; ---- flatten a single entity, recursing into inserts/polylines ----
(defun flz-flatten-ent (en / elist etype sub blkname attflag pn)
  (if (and en (setq elist (entget en)))
    (progn
      (setq etype (cdr (assoc 0 elist)))
      (cond

        ;; INSERT (and MINSERT): flatten insertion pt, attribs, then
        ;; recurse into the block definition
        ((= etype "INSERT")
         (flz-safe-entmod (flz-zero-point-and-elev elist nil))
         (setq blkname (cdr (assoc 2 elist))
               attflag (cdr (assoc 66 elist)))
         (if (= attflag 1)
           (progn
             (setq sub (entnext en))
             (while (and sub (/= (cdr (assoc 0 (entget sub))) "SEQEND"))
               (flz-flatten-ent sub)
               (setq sub (entnext sub)))))
         (flz-flatten-block blkname))

        ;; old-style POLYLINE: flatten header, then each VERTEX. Vertices
        ;; of a 2D polyline live in the header's OCS; 3D polylines and
        ;; meshes (flags 8/16/64) have WCS vertices.
        ((= etype "POLYLINE")
         (setq pn (if (= 0 (logand 88 (cdr (assoc 70 elist))))
                    (cond ((cdr (assoc 210 elist))) ('(0.0 0.0 1.0)))))
         (flz-safe-entmod (flz-zero-point-and-elev elist nil))
         (setq sub (entnext en))
         (while (and sub (/= (cdr (assoc 0 (entget sub))) "SEQEND"))
           (flz-safe-entmod (flz-zero-point-and-elev (entget sub) pn))
           (setq sub (entnext sub))))

        ;; everything else: just zero its own points/elevation
        (t
         (flz-safe-entmod (flz-zero-point-and-elev elist nil))))
      (entupd en)))
  en)

;; ---- zero the Z of a block definition's base point ----
(defun flz-zero-block-origin (blkname / blk o)
  (setq blk (vl-catch-all-apply 'vla-item
              (list (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object)))
                    blkname)))
  (if (and (not (vl-catch-all-error-p blk))
           (setq o (vlax-get blk 'Origin))
           (/= 0.0 (caddr o)))
    (vl-catch-all-apply 'vlax-put (list blk 'Origin (list (car o) (cadr o) 0.0)))))

;; ---- flatten a block DEFINITION once, recursing into nested blocks ----
(defun flz-flatten-block (blkname / bdef blkhdr en)
  (if (and blkname (not (member (strcase blkname) *flz-done-blocks*)))
    (progn
      (setq *flz-done-blocks* (cons (strcase blkname) *flz-done-blocks*))
      (setq bdef (tblsearch "BLOCK" blkname))
      (if (and bdef
               (= 0 (logand 4 (cdr (assoc 70 bdef))))   ; skip xrefs
               (setq blkhdr (tblobjname "BLOCK" blkname)))
        (progn
          (flz-zero-block-origin blkname)
          (setq en (entnext blkhdr))
          (while en
            (flz-flatten-ent en)
            (setq en (entnext en)))))))
  blkname)

;; ---- command entry points ----
(defun c:FLATTENZ (/ ss)
  (prompt "\nSelect objects to flatten Z on (nested block content included): ")
  (if (setq ss (ssget))
    (flz-flatten ss)
    (princ "\nNothing selected."))
  (princ))

(defun c:FLATTENZALL (/ ss)
  (if (setq ss (ssget "_X"))
    (flz-flatten ss)
    (princ "\nDrawing is empty."))
  (princ))

;; ---- flatten a selection set inside one undo group ----
;; *flz-done-blocks* / *flz-tilted* are locals here; dynamic scoping makes
;; them visible to flz-flatten-ent and the helpers it calls.
(defun flz-flatten (ss / i en *flz-done-blocks* *flz-tilted*
                         olderr oldecho)
  (setq olderr  *error*
        oldecho (getvar "CMDECHO")
        *flz-done-blocks* nil
        *flz-tilted* 0)
  (defun *error* (msg)
    (command-s "_.UNDO" "_END")
    (setvar "CMDECHO" oldecho)
    (setq *error* olderr)
    (if (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*"))
      (princ (strcat "\nError: " msg)))
    (princ))
  (setvar "CMDECHO" 0)
  (command "_.UNDO" "_BEGIN")
  (setq i 0)
  (while (< i (sslength ss))
    (setq en (ssname ss i))
    (flz-flatten-ent en)
    (setq i (1+ i)))
  (command "_.UNDO" "_END")
  (command "_.REGENALL")
  (setvar "CMDECHO" oldecho)
  (setq *error* olderr)
  (princ (strcat "\nFlattened " (itoa (sslength ss))
                 " selected object(s) and their nested block content."))
  (if (> *flz-tilted* 0)
    (princ (strcat "\nWARNING: " (itoa *flz-tilted*)
                   " object(s) have a genuinely tilted extrusion normal"
                   " and were left in their own plane.")))
  (princ))

;; ===================================================================
;; FLZCHECK - pick any object (also inside nested blocks) and report
;; values the Properties palette rounds away or doesn't show.
;; ===================================================================
(defun flz-vstr (v)
  (strcat "(" (rtos (car v) 1 6) " " (rtos (cadr v) 1 6)
          (if (caddr v) (strcat " " (rtos (caddr v) 1 6)) "")
          (if (cadddr v) (strcat " " (rtos (cadddr v) 1 6)) "") ")"))

(defun c:FLZCHECK (/ sel en ed m n endp par p z maxz)
  (if (setq sel (nentselp "\nPick object (nested OK): "))
    (progn
      (setq en (car sel)
            ed (entget en)
            m  (caddr sel))                ; block->WCS matrix if nested
      (prompt (strcat "\nType: " (cdr (assoc 0 ed))))
      (if (setq n (cdr (assoc 210 ed)))
        (prompt (strcat "\nOwn normal (210):    " (flz-vstr n))))
      (if (assoc 38 ed)
        (prompt (strcat "\nOwn elevation (38):  " (rtos (cdr (assoc 38 ed)) 1 6))))
      (if m
        (prompt (strcat "\nNested; WCS Z row:   " (flz-vstr (nth 2 m))
                        "\n  (first two terms /= 0 -> a parent block is tilted,"
                        " last term /= 0 -> a parent is raised)")))
      (if (not (vl-catch-all-error-p
                 (setq endp (vl-catch-all-apply 'vlax-curve-getEndParam (list en)))))
        (progn
          (setq maxz 0.0
                par  (vlax-curve-getStartParam en))
          (while (<= par endp)
            (setq p (vlax-curve-getPointAtParam en par))
            (if p
              (progn
                (setq z (if m
                          (+ (* (car (nth 2 m)) (car p))
                             (* (cadr (nth 2 m)) (cadr p))
                             (* (caddr (nth 2 m)) (caddr p))
                             (cadddr (nth 2 m)))
                          (caddr p)))
                (setq maxz (max maxz (abs z)))))
            ;; polylines: every vertex; other curves: start and end only
            (setq par (cond ((>= par endp) (1+ endp))
                            ((not (wcmatch (cdr (assoc 0 ed)) "*POLYLINE")) endp)
                            ((> (1+ par) endp) endp)
                            (t (1+ par)))))
          (prompt (strcat "\nMax |WCS Z| at vertices: " (rtos maxz 1 6)))))))
  (princ))

(princ "\nFLATTENZ loaded. Type FLATTENZ or FLATTENZALL to run, FLZCHECK to diagnose.")
(princ)
