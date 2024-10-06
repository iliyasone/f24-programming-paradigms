#lang racket

(require racket)

(define a 1664525)
(define c 1013904223)
(define m (expt 2 32))

(struct gen (state))

(displayln "Exercise 1.1.1")

(define (mk-gen seed)
  (gen seed))

(define (next-gen g)
    (gen (remainder (+ c (* a (gen-state g))) m))
)

(println (mk-gen 123)) 
; #<gen>

(println (next-gen (mk-gen 123)))
; #<gen> 

(displayln "Exercise 1.1.2")

(define ((gen-integer i j) g)
    (cons 
        (+ i (remainder (gen-state g) (- j i)))
        (next-gen g)
    )
)

(println ((gen-integer 10 34) (mk-gen 123)))
; '(13 . #<gen>)
(println ((gen-integer 10 34) (mk-gen 123)))
; '(13 . #<gen>)    the same as above

(println ((gen-integer 10 34) 
            (next-gen (mk-gen 123))
           )
)
; '(32 . #<gen>)
(println ((gen-integer 10 34) 
            (cdr ((gen-integer 10 34) (mk-gen 123)))
            )
)
; '(32 . #<gen>)    the same as above

(displayln "Exercise 1.1.3")
(require racket/stream)

(define (stream-integers n m g)
    (stream-cons 
    (car ((gen-integer n m) g))
    (stream-integers n m (next-gen g))
    )
)

(println (stream->list (stream-take (stream-integers 1 10 (mk-gen 123)) 5)))
; (7 2 7 7 4)
(println (stream->list (stream-take (stream-integers 1 10 (mk-gen 123)) 10)))
; (7 2 7 7 4 1 8 4 9 6)     the first 5 integers the same as above


(displayln "Exercise 1.1.4")

(define (stream-random f g)
    (stream-cons 
    (car (f g))
    (stream-random f (cdr (f g))) ; not (next-gen g) because f could use generator more than once 
    )
)

(println (stream->list (stream-take (stream-random (gen-integer 1 10) (mk-gen 123)) 5)))
; (7 2 7 7 4)
(println (stream->list (stream-take (stream-random (gen-integer 1 10) (mk-gen 123)) 10)))
;(7 2 7 7 4 1 8 4 9 6)


(displayln "Exercise 1.2.1")
(define (gen-bool g)
    (cons 
        (zero? (remainder (gen-state g) 2))
        (next-gen g)
    )
)

(println (stream->list (stream-take (stream-random gen-bool (mk-gen 13123)) 5)))
; '(#f #t #f #t #f)

(displayln "Exercise 1.2.2")

(define ((gen-element lst) g)
    (cons
        (list-ref lst
            (car ((gen-integer 0 (length lst)) g))
        )
        (next-gen g)
    )
)

(println (stream->list (stream-take (stream-random (gen-element '(1 2 3)) (mk-gen 13)) 5)))
; '(2 1 3 1 1)


; *all previous cells required*

(displayln "Exercise 1.2.3")

(define ((gen-pair f1 f2) g)
    (cons 
        (cons 
            (car (f1 g))
            (car (f2 (cdr (f1 g))))
        )
        (cdr (f2 (cdr (f1 g))))
    )
)

(println (stream->list (stream-take (stream-random (gen-pair gen-bool gen-bool) (mk-gen 31337)) 3)))
; '((#f . #t) (#f . #t) (#f . #t))

(stream->list (stream-take (stream-random (gen-pair (gen-integer 1 10) (gen-integer 1 10)) (mk-gen 31337)) 3))
; '((9 . 3) (1 . 5) (8 . 8))


(displayln "Exercise 1.2.4")


(define (stream-random-full f g)
    (stream-cons 
    (cons (car (f g)) (cdr (f g)))
    (stream-random-full f (cdr (f g))) 
    ;                     ^^^^^^^^^^^ 
    ; not (next-gen g) because `f` could use
    ; generator more than once 
    )
)

(define ((gen-list f n) g)
    (let ([lst (stream->list (stream-take (stream-random-full f g) n))])
    (cons
        (for/list ([pair lst])
            (car pair)
        )
        (cdr (list-ref lst (- n 1)))
    )
    )
)
; '(7 2 7 7 4 1 8 4 9 6)


(println ((gen-list (gen-integer 1 10) 10) (mk-gen 123)))

(stream->list (stream-take (stream-random (gen-integer 1 10) (mk-gen 123)) 10))

(displayln "Exercise 1.2.5")

(define ((gen* . fs) g)
    (let-values (
        [
            (result final-g)
            (for/fold ([result empty]
                    [g g])  ; local vars
                    ([f fs])
            (values 
            (append result (list (car (f g))))
            (cdr (f g))))
        ]
    )
    (cons result final-g))
)




(println ((gen*) (mk-gen 123)))
; '(() . #<gen>)

(println ((gen* (gen-integer 1 10)) (mk-gen 123)))
; '((7) . #<gen>)

(println ((gen* (gen-integer 1 10) (gen-integer 10 20)) (mk-gen 123)))
; '((7 18) . #<gen>)


(displayln "Exercise 1.2.6")

(define ((gen-struct user-struct . fs) g)
    (cons
        (apply user-struct (car ((apply gen* fs) g)))
        (cdr ((apply gen* fs) g))
    )
)

(struct person (name age))
(let [(p ((gen-struct person
(gen-element '(Anna Boris Charlie))
(gen-integer 20 30))
(mk-gen 123)))]
(displayln (person-name (car p)))
(displayln (person-age (car p)))
(displayln (cdr p)))

(displayln "Exercise 1.2.7")

(define (gen-int->int g)
    (cons

        (lambda (n)
            (+ 
                (* n 
                    (car ((gen-integer 0 10) g))
                ) 
                (car ((gen-integer 0 10) (next-gen g)))
            )
        )
        (next-gen (next-gen g))
    )
)
(map (lambda (f) (f 5))
(stream->list (stream-take (stream-random gen-int->int (mk-gen 4)) 10)))


(struct property (generator predicate))

(displayln "Exercise 1.3.1")

(define (stream-counterexamples-infinity property g)
    (cond 
        [
            (not 
                ((property-predicate property) 
                 (car ((property-generator property) g)))
            )
            (
            (stream-cons
                (car ((property-generator property) g))
                (stream-counterexamples-infinity property (next-gen g))
            )
            )
        ]
        [else 
        (stream-cons
            0
            (stream-counterexamples-infinity property (next-gen g))
        )
        ]
    )
)

(define (stream-counterexamples property g)
    (stream-take
        (stream-counterexamples-infinity property g)
        100
    )
)


(stream->list 
    (stream-counterexamples (property (gen-integer 1 100) (lambda (x) (> x 5)))
(mk-gen 123)))
