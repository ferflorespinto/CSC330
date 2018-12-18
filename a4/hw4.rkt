#lang racket

(provide (all-defined-out))

; Name: Jorge Fernando Flores Pinto
; ID: V00880059
; CSC330 - Fall 2018
; Assignment 4

; Takes three arguments low, high, and stride, all assumed to be numbers. Further,
; we assume stride is positive. sequence produces a list of numbers from low to
; high (including low and possibly high) separated by stride and in sorted order.
(define (sequence low high stride)
  (if (> low high)
      null
      (range low (+ high 1) stride))
)

; Takes a list of strings xs and a string suffix and returns a list of strings.
; Each element of the output is the corresponding element of the input appended
; with suffix (with no extra space between the element and suffix).
(define (string-append-map xs suffix)
    (map (lambda (str) (string-append str suffix)) xs)
)

; Takes a list xs and a number n. If the number is negative, it terminates the
; computation with (error "list-nth-mod: negative number"). Else if the list is empty,
; it terminates the computation with (error "list-nth-mod: empty list"). Else, returns the
; i-th element of the list where we count from zero and i is the remainder produced
; when dividing n by the list’s length.
(define (list-nth-mod xs n)
  (if (negative? n)
      (error "list-nth-mod: negative number")
      (if (null? xs)
          (error "list-nth-mod: empty list")
          (let* ([r (remainder n (length xs)) ])
            (car (list-tail xs r))
          ) 
      ))
)

; Takes a stream s and a number n. It returns a list holding the first n values produced
; by s in order.
(define (stream-for-n-steps s n)
    (if (= n 0)
        null
        (if (= n 1)
            (list (car (s)))
            (append (list (car (s))) (stream-for-n-steps (cdr (s)) (- n 1)))
        ))
)

; Like the stream of natural numbers (i.e., 1, 2, 3, ...), except numbers divisible by 5
; are negated (i.e., 1, 2, 3, 4, -5, 6, 7, 8, 9, -10, 11, ...).
(define funny-number-stream
    (letrec ([f (lambda (x)
                  (if (= (modulo x 5) 0)
                      (cons (* x -1) (lambda () (f (+ x 1))))
                      (cons x (lambda () (f (+ x 1))))
                   )
             )])
      (lambda () (f 1)))
)

;  The elements of the stream alternate between the strings "cat.jpg" and "dog.jpg"
; (starting with "cat.jpg"). 
(define cat-then-dog
    (letrec ([f (lambda (x)
                  (if (= (modulo x 2) 0)
                      (cons "dog.jpg" (lambda () (f (+ x 1))))
                      (cons "cat.jpg" (lambda () (f (+ x 1))))
                   )
            )])
      (lambda () (f 1)))
)

;  Takes a stream s and returns another stream. If s would produce v for its i-th element,
; then (stream-add-zero s) would produce the pair (0 . v) for its i-th element.
(define (stream-add-zero s)
    (letrec ([f (lambda (x)
                 (cons (cons 0 (car (x))) (lambda () (f (cdr (x)))))
            )])
      (lambda () (f s)))
)

; Takes two lists xs and ys and returns a stream. The lists may or may not be the same length,
; but we assume they are both non-empty. The elements produced by the stream are pairs where the
; first part is from xs and the second part is from ys. The stream cycles forever through the lists.
(define (cycle-lists xs ys)
    (letrec ([f (lambda (x y)
                  (cons (cons (car x) (car y))
                    (lambda () (f (append (cdr x) (list (car x))) (append (cdr y) (list (car y))))))
             )])
      (lambda () (f xs ys)))
)

; Takes a value v and a vector vec. It behaves like Racket’s assoc library function except:
; (a) it processes a vector instead of a list and
; (b) it allows vector elements not to be pairs in which case it skips them.
; It processes the vector elements in order starting from 0. Returns #f if no vector element is
; a pair with a car field equal to v, else returns the first pair with an equal car field.
(define (vector-assoc v vec)
    (letrec ([f (lambda (pos)
                  (cond [(>= pos (vector-length vec)) #f]
                        [(pair? (vector-ref vec pos))
                           (if (equal? v (car (vector-ref vec pos)))
                               (vector-ref vec pos)
                               (f (+ pos 1)))]
                        [#t (f (+ pos 1))])
                  )])
      (f 0))
)

; Takes a list xs and a number n and returns a function that takes one argument v and returns
; the same thing that (assoc v xs) would return. However, it uses an n-element cache of
; recent results. It first looks up for the element in the cache, and if found, returns the
; element. Else, it performs (assoc v xs), and adds the element found to the cache.
(define (cached-assoc xs n)
    (letrec ([cache (make-vector n #f)]
             [pos 0]
             [update-cache (lambda (v) (begin
                             ;(print "updating cache")
                             (vector-set! cache pos (assoc v xs))
                             (set! pos (add1 pos))
                             (vector-ref cache (sub1 pos))))])
      (lambda (v)
        (cond [(>= pos n) (set! pos 0)]
              [#t (if (vector-assoc v cache)
                      (vector-assoc v cache)
                      (update-cache v)
                   )])))                      
)

; A macro that behaves like the "do-while" loops in C and Java. It executes the expression in
; the 'do' part at least once, and then enters the 'while' part to check if a condition is met.
; The loop continues until the 'while' condition is no longer met.
(define-syntax while-less
  (syntax-rules (do)
    [(while-less hi do body)
     (let ([h hi])
       (letrec ([loop (lambda (it)
                        (if (>= it h)
                            #t
                            (begin (loop body))))])
         (begin (loop body))))]))