#lang racket
(struct Tree(v left right) #:transparent)
(struct EmptyT() #:transparent)
(define funny-number-stream
    (letrec ([f (lambda (x)
                  (if (= (modulo x 5) 0)
                      (cons (* x -1) (lambda () (f (+ x 1))))
                      (cons x (lambda () (f (+ x 1))))
                   )
             )])
      (lambda () (f 1)))
)

(define (drop n s)
  ;;(letrec ([f (lambda(x)
                (if (= n 0)
                    s
                    (drop (- n 1) (cdr (s)))))
    ;;(lambda () (f n))))

(define (tree-map f tree)
  (if (EmptyT? tree)
      tree
      (letrec([left (Tree-left tree)]
              [right (Tree-right tree)])
        (if (Tree? tree)
               (Tree (f (Tree-v tree)) (tree-map f left) (tree-map f right))
               tree
              ))))

; (define t1 (Tree 5 (Tree 4 (EmptyT) (EmptyT)) (Tree 10 (EmptyT) (EmptyT))))
; (tree-map (lambda (v) (* v 2)) t1)
(define f
  (let ([y 0])
    (lambda ()
      (begin (set! y (+ y 1))
             (let ([x y])
               (lambda () x))))))
(define ans (list ((f)) ((f)) ((f))))