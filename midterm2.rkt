#lang racket
;1 (a)
(define (f p)
  (let (
        (x 5)
        (y 4)
        (z (car p))
        (w (cdr p))
        )
    ;;(+ 9 5) = 14
    (+ (z (w y)) x)))
(define x 1)
(define y 1)
(define ans (f (cons (lambda (z) (+ x z))
                     (lambda (x) (+ x x 0))
                     )))

;1 (b)
(define x 3)
;(g 3)
(define f (lambda (y) (y x)))
(define g
  (let ((x 7))
    ; (7 - 3) = 4
    (lambda (y) (- x y))))
(define ans (f g))

;1 (c)
(define (f x) (map cdr x))
; map cdr (list (list 1 2 3) (list 4 5))
; (list (list (2 3) list(5))
(define ans (f (list (list 1 2 3) (list 4 5))))

(define (create_stream fs init)
 (letrec ([st (lambda (x)
                (cons (fs x) (lambda () (fs (+ init 1)))))])
   (lambda () (st init))))
                
(define (first_n_values s n)
  (letrec([count 1]
          [f (lambda (x c)
               