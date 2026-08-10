(defun c:AlOnSegment
  (/ ss ent1 ent2
     pt1 pt2 pt3 pt4
     parent parents
     ang1 ang2
     tol
     parallel
     horizontal
     ux uy
     dx dy
     along
     moveX moveY
     perp dist)

  (setq tol 1e-3)

  ;; ------------------------------------------------------------
  ;; Select target object
  ;; ------------------------------------------------------------

  (setq ent2 (entsel "\nSelect an object to align to"))

  (if ent2
    (progn      
      (setq ent1 (nentsel "\nSelect an object to align"))

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

        (setq pt1 (osnap (cadr ent1) "_end"))
        (setq pt3 (osnap (cadr ent1) "_nea"))
        (setq pt2 (osnap (cadr ent2) "_nea"))
        (setq pt4 (osnap (cadr ent2) "_end"))

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
