(load "obj.scm")

(define (line start end steps) (let ((div (- steps 1)) (dist (- end start))) (map (lambda (x) (+ start (* dist (/ x div)))) (range steps))))

(define (line2d start end steps) (let ((div (- steps 1)) (dist (cons (- (car end) (car start)) (- (cdr end) (cdr start)) ))) (map (lambda (x) (cons (+ (car start) (* (car dist) (/ x div))) (+ (cdr start) (* (cdr dist) (/ x div))))) (range steps))))


;;(load "walls.scm") I've commented this out because my walls library isn't done yet.

(define (merge-faces a b)
  (faces
   (+ (car a) (car b))
   (append (cadr a) (cadr b)) (append (cddr a) (map (lambda (x) (map (lambda (y) (+ y (car a))) x)) (cddr b))))) 

(define (faces vc v f) (cons vc (cons v f)))

(define (faces->obj f) (.obj (cadr f) (cddr f)))

(define (mean-angle a1 a2) (let ((mean (/ (+ a1 a2) 2))) (if (> (abs (- mean a1)) (* .5 pi)) (mod (+ mean pi) tau) mean)))

(define (cross-section-vertices x magnitude rot-res) (maprange (lambda (a)
                                                          ((lambda (angle) (3vector x (* magnitude (cos angle)) (* magnitude (sin angle)) )) (* 2 pi (/ a rot-res)) )) rot-res))

(define (faces-of-revolution len rot-res depth) (if (= 1 len) '() (append
                                                              (maprange (lambda (x) `(,(+ x 1 (* depth rot-res)) ,(+ x 2 (* depth rot-res)) ,(+ rot-res (* depth rot-res) x 1) ,(+ rot-res (* depth rot-res) x))) rot-res)
                                                              (faces-of-revolution (- len 1) rot-res (+ depth 1)))))

(define (vertices-of-revolution coordinates rot-res)
  (if (null? coordinates) '() (append (cross-section-vertices (caar coordinates) (cdar coordinates) rot-res)
                                      (vertices-of-revolution (cdr coordinates) rot-res))))

(define (surface-of-revolution coordinates rot-res)
(let ((vertices (vertices-of-revolution coordinates rot-res))
  (faces (faces-of-revolution (length coordinates) rot-res 0))
  )
 (.obj vertices faces)
  ))


(define (function-of-revolution function start end res rotres closeend)
  (surface-of-revolution (append (maprange (lambda (x)
              (let ((pos (+ start (* (/ x (- res 1)) (- end start)))))
                (cons pos (function pos) ))) res) (if closeend (list (cons end 0)) '()))
           rotres
  ))

(define (dynamic-function-of-revolution function start end res rotres)
  (dynamic-surface-of-revolution (maprange (lambda (x)
              (let ((pos (+ start (* (/ x (- res 1)) (- end start)))))
                (cons pos (function pos) ))) res)
           rotres
  ))


(define (angle-dependant-generatrix f) (lambda (angle) (f angle)))

(define (bi-generatrix condition f1 f2)
  (angle-dependant-generatrix
   (lambda (angle) (if (condition angle) f1 f2))))

(define (bi-generatrix-coordinates condition g1 g2)
  (if (= (length g1) (length g2))
  (angle-dependant-generatrix
   (lambda (angle) (if (condition angle) g1 g2)))))

(define bigen (bi-generatrix (lambda (angle) (< (/ pi 6) (mod angle (/ pi 3)))) sin (lambda (x) (* (sin x) .2 ))))
;(define bigen (bi-generatrix-coordinates (lambda (angle) (< (/ pi 6) (mod angle (/ pi 3)))) '(0 1 4 2) '(0 2 3 4)))

(define (bowl-function width height thickness power)
  (function-of-revolution (lambda (x) (expt (sin (acos x)) power)) 0 1 24 128))


(define (cup-coordinates height radius thickness)
  `((0 . 0)
    (0 . ,radius)
    (,height . ,radius)
    (,height . ,(- radius thickness))
    (,thickness . ,(- radius thickness))
    (,thickness . 0)
    ))

(define (bowl-coordinates height radius exp1 exp2 thickness steps)
  (maprange (lambda (x)
              (cons (* height (expt (sin (+ (/ pi 2) (* (/ x steps) (/ pi 2)))) exp1))  (* 1.0 (expt (/ x steps) exp2) radius))) (+ steps 1)))


(define (cut-gem-coordinates height radius) `((0 . 0) (,(* height .7) . ,radius) (,height . ,(* radius .7)) (,height . 0)))

(define (sum-2-lists list1 list2) (if (null? list1)
                                    '()
                                    (cons
                                     (+ (car list1) (car list2))
                                     (sum-2-lists (cdr list1) (cdr list2)))))


(define x-axis (lambda (x) `(,x 0 0)))

(define y-axis (lambda (x) `(0 ,x 0)))

(define z-axis (lambda (x) `(0 0 ,x)))

(define (azimuth-to-axis angle) ((lambda (xval yval) (lambda (x) `(,(* xval x) ,(* yval x) 0))) (cos angle) (sin angle)))

(define (plane origin x-func y-func)
  (lambda (x y) (apply 3vector (sum-2-lists origin (sum-2-lists (x-func x) (y-func y))))))

(define (plane-roll origin x-func y-func . roll)
  (lambda (x y) (apply 3vector (sum-2-lists origin (sum-2-lists
                                                    (x-func (+ (* (cos (car roll)) x) (* (cos (+ (car roll) (* pi .5))) y))) (y-func (+ (* (sin (car roll)) x) (* (sin (+ (car roll) (* pi .5))) y))))))))

(define (extrude2d-section path generatrix)
(if (null? generatrix)
'()
(cons (path (caar generatrix) (cdar generatrix)) (extrude2d-section path (cdr generatrix)))
))


(define (extrude2d-vertices path generatrix)
  (if (null? path)
      '()
      (append (extrude2d-section (car path) generatrix)  (extrude2d-vertices (cdr path) generatrix))
  ))

(define (extrude2d-faces-section genlength n)
  (maprange
   (lambda (x) `(,(+ n x) ,(+ n genlength x) ,(+ n genlength 1 x) ,(+ n 1 x))) (- genlength 1)))

(define (dynamic-extrude2d-vertices path generatrix step totalsteps)
  (if (null? path)
      '()
      (append
       (extrude2d-section (car path) (generatrix step (/ step totalsteps)))
       (dynamic-extrude2d-vertices (cdr path) generatrix (+ step 1) totalsteps))
  ))

(define (switch-axis l) (map (lambda (x) (cons (cdr x) (car x))) l))
(define (extrude2d-faces pathlength genlength n)
  (if (= pathlength 0)
      '()
      (append (extrude2d-faces-section genlength n) (extrude2d-faces (- pathlength 1) genlength (+ n genlength)))))

(define (extrude2d-triangular-cap-section genlength pathpos n) (let ((startpos (+ (* genlength pathpos) 1))) `(,(+ startpos n)
                                                    ,(+ 1 n startpos)
                                                    ,(+ startpos (- genlength n 3))
                                                    ,(+ startpos (- genlength n 2)))))

(define (extrude2d-triangular-cap genlength pathpos)
   (maprange
            (lambda (x) (extrude2d-triangular-cap-section genlength pathpos x))
            (floor (- (/ (- genlength 1) 2) 1))))

;Create a face from a list of vertices
;(define (extrude2d-cap genlength pathpos)
;   (let ((startpos (+ (* genlength pathpos) 1)))
;(list (map (lambda (x) (+ x startpos)) (range genlength))))
;     )

(define extrude2d-cap extrude2d-triangular-cap)





(define (extrude2d path generatrix startcap endcap)
  (let ((vertices (extrude2d-vertices path generatrix)))

(faces->obj 
(faces (length vertices) vertices
      (append
                                   (extrude2d-faces (- (length path) 1) (length generatrix) 1)
(if startcap (extrude2d-cap (length generatrix) 0) '())
(if endcap (extrude2d-cap (length generatrix) (- (length path) 1)) '()))
  ))))




(define (dynamic-extrude2d path genfunc)
(.obj (dynamic-extrude2d-vertices path genfunc 0 (- (length path) 1)) (extrude2d-faces (- (length path) 1) (length (genfunc 0 0)) 1)
  ))


(define (path2d-angles points)
(if (null? (cdr points))
    '()
  (let ((currentline (+ (/ pi 2) (atan-points (car points) (cadr points)))))
    (cons currentline (path2d-angles (cdr points)))
                                 )))

(define (pathpoints->origin point) `(,(car point) ,(cdr point) 0))
(define (open-path-angles->planes points angle last-point) (if (null? angle)
                        ;;The last point
                                        (cons (plane (pathpoints->origin (car points)) (azimuth-to-axis last-point) z-axis) '())
                                        (if (null? last-point)
                        ;;The first point
                                        (cons (plane (pathpoints->origin (car points)) (azimuth-to-axis (car  angle)) z-axis) (open-path-angles->planes (cdr points) (cdr angle) (car  angle)))
                        ;;other points
                                        (cons (plane (pathpoints->origin (car points)) (azimuth-to-axis (mean-angle (car  angle) last-point)) z-axis) (open-path-angles->planes (cdr points) (cdr angle) (car  angle)))
                                           )
  ))

(define (closed-path-angles->planes points angle last-point first-point) (if (null? angle)
                        ;;The last point
                                        (cons (plane (pathpoints->origin (car points)) (azimuth-to-axis (mean-angle last-point first-point)) z-axis) '())
                                        (if (null? last-point)
                        ;;The first point
                                        (cons (plane (pathpoints->origin (car points)) (azimuth-to-axis (car  angle)) z-axis) (closed-path-angles->planes (cdr points) (cdr angle) (car  angle) first-point))
                        ;;other points
                                        (cons (plane (pathpoints->origin (car points)) (azimuth-to-axis (mean-angle (car  angle) last-point)) z-axis) (closed-path-angles->planes (cdr points) (cdr angle) (car  angle) first-point))
                                           )
  ))

  (define (path-angles->planes points path closed)
    (if closed
    (closed-path-angles->planes points path (final-element path) (car path))    
    (open-path-angles->planes points path '())))

(define (open-path-angles-rolls->planes points angle rolls last-point) (if (null? angle)
                        ;;The last point
                                        (cons (plane-roll (pathpoints->origin (car points)) (azimuth-to-axis last-point) z-axis (car rolls)) '())
                                        (if (null? last-point)
                        ;;The first point
                                        (cons (plane-roll (pathpoints->origin (car points)) (azimuth-to-axis (car  angle)) z-axis (car rolls)) (open-path-angles-rolls->planes (cdr points) (cdr angle) (cdr rolls) (car  angle)))
                        ;;other points
                                        (cons (plane-roll (pathpoints->origin (car points)) (azimuth-to-axis (mean-angle (car  angle) last-point)) z-axis (car rolls)) (open-path-angles-rolls->planes (cdr points) (cdr angle) (cdr rolls) (car  angle)))
                                           )
  ))

(define (closed-path-angles-rolls->planes points angle rolls last-point first-point) (if (null? angle)
                        ;;The last point
                                        (cons (plane-roll (pathpoints->origin (car points)) (azimuth-to-axis (mean-angle last-point first-point)) z-axis (car rolls)) '())
                                        (if (null? last-point)
                        ;;The first point
                                        (cons (plane-roll (pathpoints->origin (car points)) (azimuth-to-axis (car  angle)) z-axis (car rolls)) (closed-path-angles-rolls->planes (cdr points) (cdr angle) (cdr rolls) (car  angle) first-point))
                        ;;other points
                                        (cons (plane-roll (pathpoints->origin (car points)) (azimuth-to-axis (mean-angle (car  angle) last-point)) z-axis (car rolls)) (closed-path-angles-rolls->planes (cdr points) (cdr angle) (cdr rolls) (car  angle) first-point))
                                           )
  ))

  (define (path-angles-rolls->planes points path rolls closed)
    (if closed
        (closed-path-angles-rolls->planes points path rolls (final-element path) (car path))
    (open-path-angles-rolls->planes points path rolls '())))
    
(define (path2d points closed)
(let ((angles (path2d-angles points)))
  ;(if closed
(path-angles->planes points angles closed)
  ))

(define (path-rolls2d points rolls closed)
(let ((angles (path2d-angles points)))
  ;(if closed
(path-angles-rolls->planes points angles rolls closed)
  ))

(define testpath (path2d '((0 . 0) (2 . 0) (3 . 1) (4 . 3) (7 . 10) (7 . 12)) #t))

(define spiralpath (path2d (maprange (lambda (x) (cons (* x .1 (cos (* x (/ tau 64)))) (* x .1 (sin (* x (/ tau 64)))))) 128) #f))

(define (ellipsegen wr hr steps)
(let ((stepang (/ pi .5 steps)))
  (maprange (lambda (n) (cons (* wr (cos (* n stepang))) (* hr (sin (* n stepang))))) (+ 1 steps)) ))


(define (housewalls w l h doorsize)
 (extrude2d
          (path2d `((0 . 0) (,(/ h 2) . 0) (,h . 0)) #f)
          (lines->polygon `((,(- 0 w) . ,0) (,(- 0 w) . ,l)(,w . ,l) (,w . ,(- 0 l)) (,(- 0 w) . ,(- 0 l)) (,(- 0 w) . ,(- 0 doorsize))) .6)
          #t
          #t)
 )

