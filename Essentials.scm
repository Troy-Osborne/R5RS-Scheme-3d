(#%require srfi/27)


;;This section from http://www.lee-mac.com/mathematicalfunctions.html
(define (sinh x )
    (/ (- (exp x) (exp (- x))) 2.0)
)

;; Hyperbolic Cosine  -  Lee Mac
;; Args: x - real

(define (cosh x )
    (/ (+ (exp x) (exp (- x))) 2.0)
)

;; Hyperbolic Tangent  -  Lee Mac
;; Args: x - real

(define (tanh x )
    (/ (sinh x) (cosh x))
)

;; Area Hyperbolic Sine  -  Lee Mac
;; Args: x - real

(define (asinh  x )
    (log (+ x (sqrt (1+ (* x x)))))
)

;;;This point on is back to Trogramming content

(define None 'None)
(define Undefined 'undefined)

(define (zero->one steps) (map (lambda (x) (/ x steps)) (range (+ steps 1))))

(define (distance2d point1 point2) (sqrt (+ (expt (- (car point2) (car point1)) 2) (expt (- (cdr point2) (cdr point1)) 2))))
;;Get distance of 2d point encoded as a cons cell

(define (distance-inner a b) (if (or (null? a) (null? b)) 0 (+ (expt (- (car a) (car b)) 2) (distance-inner (cdr a) (cdr b)))))
;;Sum the difference between a and b squared.

(define (distance point1 point2) (sqrt distance-inner a b))
;;N Dimensional Distance Function for null terminated lists

(define (angle-distance angle1 angle2) (let ((dist (mod (- (mod angle1 tau) (mod angle2 tau)) tau)))
 (if (< dist pi) dist (- tau dist))))

(define (angle-difference angle1 angle2)
  (let ((dist (mod (- (mod angle2 tau) (mod angle1 tau)) tau)))
                                         (if (< dist pi) dist (- dist tau))))


(define (mod a b) (* b (- (/ a b) (floor (/ a b)))))

(define (remove-if c l) (if (null? l) '() (if (c (car l)) (remove-if c (cdr l)) (cons (car l) (remove-if c (cdr l))))))

(define pass 'pass)

(define pi (* 4 (atan 1.0)))

(define tau (* pi 2))
(define halfpi (* .5 pi))

(define (filter f l) (if (null? l) '() (cons (f (car l)) (filter f (cdr l)))))

(define (repeat-string string n) (if (zero? n) "" (string-append string (repeat-string string (- n 1)) )))

(define (flatten l) (apply append (map (lambda (x) (if (list? x) x (list x))) l)))

(define (index-2d l pos) (index (index l (car pos)) (cdr pos)))

(define (flatten-tree l) pass)

(define (flatten-tree-withmarkers l) pass)

(define (mcar l n) (if (zero? n)
                       '()
                       (cons
                         (car l)
                         (mcar
                           (cdr l)
                           (- n 1)))))

(define (in a b) (if (null? b) #f (if (equal? (car b) a) #t (in a (cdr b)))))

(define (dictionary . rest) rest)

(define (lookup dict key) (if (null? dict) '() (if (eq? (caar dict) key) (cdar dict) (lookup (cdr dict) key))))

(define (remove-duplicate-keys dict keys) (if (null? dict)
                                              '()
                                              (if (in (caar dict) keys)
                                                  (remove-duplicate-keys (cdr dict) keys)
                                                  (cons
                                                   (car dict)
                                                   (remove-duplicate-keys (cdr dict) (cons (caar dict) keys))))))

(define (replace-key dictionary key value)
  (if (null? dictionary)
      '()
      (if (equal? (caar dictionary) key)
          (cons (cons key value) (cdr dictionary))
          (cons (car dictionary) (replace-key (cdr dictionary) key value)))))

(define (new-key dictionary key value)
(cons (cons key value) dictionary)
  )

(define (in-dict key dict)
  (if (null? dict)
      #f
      (if (equal? (caar dict) key)
       #t
       (in-dict key (cdr dict)))))

(define testdict (dictionary '("a" . 1) '("b" . 2) '("c" . 3)))

(define (dictionary-merge d1 d2) (remove-duplicate-keys (append d1 d2) '()))

(define (dictionary-assign dict key value)
(if (in-dict key dict)
    (replace-key dict key value)
    (new-key dict key value)
    ))



(define (mcdr l n) (if (zero? n)
                       l
                       (mcdr (cdr l) (- n 1))
                       ))

(define (index l n) (if (zero? n)
                       (car l)
                       (index (cdr l) (- n 1))
                       ))


(define (ainb a b) (if (null? b) #f (if (equal? (car b) a) #t (ainb a (cdr b)))))

(define (random-indices l n . occupied) (if (zero? n) '()
                                            ((lambda (x)
                                               (if (ainb x occupied)
                                                   (random-indices l n occupied)
                                                   (cons
                                                          (cons x (index l x))
                                                          (random-indices l (- n 1) (append occupied (cons x '()))))))
                                               (random-integer (length l)))))

(define (maprangeinner f t n) (if (or (< n 0) (zero? n)) '() (cons (f (- t n)) (maprangeinner f t (- n 1)))))

(define (maprange f n) (maprangeinner f n n))


(define (replace l n value) (append (mcar l n) (list value) (mcdr l (+ n 1))))

(define (increment-index l n) (replace l n (+ 1 (index l n))))

(define (map-index l n f) (replace l n (f (index l n))))

(define (incrementlast l) (append (mcar l (- (length l) 1)) (list (+ 1 (index l (- (length l) 1))))))

(define (pos-sort l) l)

(define (replace2d l coordinates value)
  (append (mcar l (car coordinates))  (list (replace (index l (car coordinates)) (cdr coordinates) value)) (mcdr l (+ (cdr coordinates) 1))))

(define (replace-indices-inner l pos-value-pairs position) (if (null? l) '()
                                                               (if (null? pos-value-pairs)
                                                                   (cons (car l) (replace-indices-inner (cdr l) '() (+ 1 position)))
                                                                   (if (equal? position (caar pos-value-pairs))
                                                                       (cons (cdar pos-value-pairs) (replace-indices-inner (cdr l) (cdr pos-value-pairs) (+ 1 position)))
                                                                       (cons (car l) (replace-indices-inner (cdr l) pos-value-pairs (+ 1 position)))
                                                                   )
                                                               )))

(define (filter f l) (if (null? l) '() (if (f (car l)) (cons (car l) (filter f (cdr l))) (filter f (cdr l)))))

(define (filter-opening l opening) (filter (lambda (x) (equal? (mcar x (length opening)) opening)) l))

(define (filter-out-opening l opening) (filter (lambda (x) (not (equal? (mcar x (length opening)) opening))) l))

(define (sign x) (if (> x 0) 1 (if (< x 0) -1 0)))
(define (square_wave x) (sign (sin x)))
(define (circle_wave x) (* (square_wave x) (sin (acos (- (mod (/ (* 2 x) pi) 2) 1) ))))
;(filter-out-opening '((1 1) (2 2) (4 3 4) (4 0)) '(4))

(define (final-element l) (index l (- (length l) 1)))
(define tau (* 2 pi))
(define (atan2 y x) (if (= 0 x) (if (> y 0) (* .5 pi) (* 1.5 pi))  (mod (if (< x 0) (+ pi (atan (/ y x))) (atan (/ y x))) tau)))

(define (atan-points p1 p2) (atan2 (- (cdr p2) (cdr p1)) (- (car p2) (car p1))))

(define (replace-indices l pos-value-pairs) (replace-indices-inner l (pos-sort pos-value-pairs) 0))

(define (2darray xlen ylen contents) (maprange (lambda (a) (maprange (lambda (b) contents) ylen)) xlen))

(define (range n) (maprange (lambda (x) x) n))