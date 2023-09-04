(load "Vectors.scm")
(load "Essentials.scm")

(define (round_number->string n) (number->string (if (integer? n) n (exact->inexact (/ (round (* n 1000) ) 1000)))))
(define (obj-vertex vertex)
  (string-append "v "
                 (round_number->string (3V_x vertex)) " "
                 (round_number->string (3V_y vertex)) " "
                 (round_number->string (3V_z vertex)) "\n"))

(define (obj-vertices vertices)
  (if (null? vertices)
      "\n"
      (string-append (obj-vertex (car vertices)) (obj-vertices (cdr vertices) ))))


;; If given a quad, turn it into two triangles, otherwise assume it's already a triangle
(define (obj-face face bothsides) (if (= 4 (length face))
                                      (let ((face1 (list (car face) (cadr face) (caddr face)))
                                            (face2 (list (car face) (caddr face) (cadddr face))))
                                        (string-append (obj-triangle face1 bothsides) (obj-triangle face2 bothsides) ))
                                      (obj-triangle face bothsides)
                                      ))


;Create a face from a list of vertices without breaking it into smaller polygons
;(define (obj-face face bothsides)
;  (apply string-append (append '("f ") (map (lambda (x) (string-append (round_number->string x) " ")) face) '("\n")
;(list (if bothsides (obj-face (reverse face) #f) "")))
;                               ))





(define (obj-triangle face bothsides)
  (string-append "f "
                 (round_number->string (car face)) " "
                 (round_number->string (cadr face)) " "
                 (round_number->string (caddr face)) "\n"
                 (if bothsides (string-append "f "
                 (round_number->string (caddr face)) " "
                 (round_number->string (cadr face)) " "
                 (round_number->string (car face)) "\n"
                 ) "")))

(define (obj-faces faces)
  (if (null? faces)
      "\n"
      (string-append (obj-face (car faces) #t) (obj-faces (cdr faces)))))

(define (.obj vertices faces) (string-append "#vertices \n" (obj-vertices vertices) "#faces \n"  (obj-faces faces)))