#lang slideshow

(define (render-bit n)
  (cond
    [(= n 1) (filled-rectangle 100 100)]
    [(= n 0) (rectangle 100 100)]
  )
)

(define space 
    (rectangle 15 100 #:border-color "white")
)

; Ex. 2.3
(define (render-bits lst)
  (cond
    [(empty? lst) (rectangle 0 0)]
    [else (
        hc-append 
        (render-bit (first lst)) 
        space
        (render-bits (rest lst)))]
  )
)

; (slide (render-bits (list 0 1 0 1 0 1 0)))


; Ex. 2.4
(define (counts-one bits) 
    (define (helper bits curr)
        (cond
            [(empty? bits) curr]
            [else (helper (rest bits) (+ (first bits) curr))]
        )
    )
    (helper bits 0)
)


; (display (counts-one '(1 0 1 0 1 1 1)))
; (newline)

; Ex 2.5

(define (tail-ones bits) 
    (define (helper bits curr)
        (cond
            [(empty? bits) curr]
            [(= (first bits) 1) (helper (rest bits) (+ 1 curr))]
            [else (helper (rest bits) 0)]
        )
    )
    (helper bits 0)
)


(define (trailing-zeros bits) 
    (define (helper bits curr)
        (cond
            [(empty? bits) curr]
            [(= (first bits) 0) (helper (rest bits) (+ 1 curr))]
            [else (helper (rest bits) 0)]
        )
    )
    (helper bits 0)
)


(display "Exercise 2.4: trailing-zeros")
(newline)
(display (trailing-zeros '(1 0 1 0 1 1 0 0 1 0 0)))
(newline)



(define (increment bits)
    (define (helper bits answer buff)
        (cond
            [(empty? bits) 
                (cond 
                    [(= buff 0) answer]
                    [(= buff 1) (cons 1 answer)]
                )
            ]
            [(= (+ (first bits) buff) 2) (helper (rest bits) (cons 0 answer) 1)]
            [(= (+ (first bits) buff) 1) (helper (rest bits) (cons 1 answer) 0)]
            [(= (+ (first bits) buff) 0) (helper (rest bits) (cons 0 answer) 0)]
        )
    )
    (helper (reverse bits) (list) 1)
)

(display "Exercise 2.5: increment")
(newline)

(display (increment '(1 1)))
(newline)

(display (increment '(1 0)))
(newline)

(display (increment '(1 0 1)))
(newline)



; (define (annotate-occurrence chrs)
;     (define (helper chrs answer)
;         (cond
;             [(empty? chrs) answer]
;             [else ]
;         )
;     )
;     (helper chrs (list) )
; )

; (define (trinary-to-decimal bits)
;     (define (helper bits result digit)
;         (cond
;             [(empty? bits) result]
;             [(= (first bits) 0) (helper (rest bits) result (+ 1 digit))]
;             [(= (first bits) 1) (helper (rest bits) (+ result (* 1 (expt 3 digit))) (+ 1 digit))]
;             [(= (first bits) 2) (helper (rest bits) (+ result (* 2 (expt 3 digit))) (+ 1 digit))]
;         )
;     )
;     (helper (reverse bits) 0 0)
; )

; (display (trinary-to-decimal '(1 0 2 1 0)))
; (newline)


; (define (third-to-last bits)
;     (define (helper bits current)
;         (cond
;             [(= current 2) (first bits)]
;             [else (helper (rest bits) (+ current 1))]
;         )
;     )
;     (helper (reverse bits) 0)
; )


; (print (third-to-last '(1 1 1 0 1 1))) ; ==> 0
; (newline)
; (print (third-to-last '(s y m b o l))) ; ==> 'b


; (define (decrement bits)
;     (define (helper bits answer buff)
;         (cond
;             [(empty? bits) 
;                 (cond
;                     [(> buff 0) '(0)] ; decrement failth 
;                     [else answer]
;                 )
;             ]
;             [(and (= (first bits) 0) (> buff 0)) (helper (rest bits) (cons 1 answer) buff)]
;             [(and (= (first bits) 1) (> buff 0)) (helper (rest bits) (cons 0 answer) (- buff 1))]
;             [else (helper (rest bits) (cons (first bits) answer) 0)]
;         )
;     )
;     (define (remove-leading-zeros bits)
;         (cond
;             [(and (not (empty? (rest bits))) (= (first bits ) 0)) (remove-leading-zeros (rest bits))]
;             [(and (not (empty? (rest bits))) (= (first bits ) 1)) bits]
;             [else bits])
;     )
;     (remove-leading-zeros (helper (reverse bits) (list) 1))
; )


; (print (decrement '(1 0 1 1 0))) ; ==> '(1 0 1 0 1)
; (newline)
; (print (decrement '(1 0 0 0 0))) ; ==> '(1 1 1 1)
; (newline)
; (print (decrement '(0))) ; ==> '(0)
; (newline)


; (define (annotate-occurrence bits)
;     (define (increment-list pairs ans chr) 
;         (cond 
;             [(and (empty? pairs) (null? chr)) ans] 
;             [(and (empty? pairs) (not (null? chr))) (cons ans list(chr 1))] ; didn't found even once
;             [(= (first (first pairs)) chr) (increment-list 
;                                             (rest pairs) 
;                                             (cons ans (list chr) (+ (second (first pairs)) 1))
;                                             null
;                                             )
;             ]
;             [else (increment-list (rest pairs) (cons ans (first pairs)) chr)]
;         )
;     )
;     (increment-list )
; )

(define (annotate-occurrence symbs)
    (define (count-char symbs symb buff)  
        (cond 
            [(empty? symbs) (list symb buff)]
            [(symbol=? (first symbs) symb) (count-char (rest symbs) symb (+ buff 1))]
            [else (count-char (rest symbs) symb buff)]
        )
    )
    (define (partial-annotate-occurrence symbs left answer)
        (cond
            [(empty? left) (reverse answer)]
            [else (
                    partial-annotate-occurrence
                    (cons (first left) symbs)
                    (rest left)
                    (cons (count-char symbs (first left) 1) answer)
                    )]
        )
    )
    (partial-annotate-occurrence (list) symbs (list))
)

(define (most-frequent symbs)
    (define (helper annotate-symbs curr_max curr_symb)
        (cond
            [(empty? annotate-symbs) curr_symb]
            [else
                (let* 
                    (
                        [check_symb (first (first annotate-symbs))]
                        [check_count (second (first annotate-symbs))]
                    )

                    (cond
                        [(> check_count curr_max) (helper (rest annotate-symbs) check_count check_symb)]
                        [else (helper (rest annotate-symbs) curr_max curr_symb)]
                    )
                )
            ]
        )
    )
    (helper (annotate-occurrence symbs) 0 null)
)


; (print (most-frequent '(h e a a l l)))
; ; ==> '((h 1) (e 1) (l 1) (l 2) (o 1) (w 1) (o 2) (r 1) (l 3) (d 1))

(define (max-and-sum lst)
    (define (max-and-sum-helper lst mxs buff)
        (cond
            [(empty? lst) (list mxs buff)]
            [(or (null? mxs) (> (first lst) mxs))
                (max-and-sum-helper 
                    (rest lst) 
                    (first lst) 
                    (+ buff (first lst))
                )
            ]
            [else (max-and-sum-helper (rest lst) mxs (+ buff (first lst)))]
        )
    )
    (max-and-sum-helper lst null 0)
)

(print (max-and-sum (list 6 2 4 1)))


; 

(second (max-and-sum (list x y z)))

;

(define (max-and-sum-helper lst mxs buff)
    (cond
        [(empty? lst) (list mxs buff)]
        [(or (null? mxs) (> (first lst) mxs))
            (max-and-sum-helper 
                (rest lst) 
                (first lst) 
                (+ buff (first lst))
            )
        ]
        [else (max-and-sum-helper (rest lst) mxs (+ buff (first lst)))]
    )
)

(second (max-and-sum-helper (list x y z) null 0))

; 

(second (cond
    [(empty? (list x y z)) (list null 0)]
    [(or (null? null) (> (first (list x y z)) 0))    ; null is null so
        (max-and-sum-helper                          ; calls `max-and-sum-helper` wit 
            (rest (list x y z))                      ; lst = (list y z)
            (first (list x y z))                     ; mxs = x
            (+ 0 (first (list x y z)))               ; buff = x
        )
    ]
    [else (max-and-sum-helper (rest (list x y z)) null (+ 0 (first (list x y z))))]
    )
)

; 

(second (cond
        [(empty? (list y z)) (list x x)]
        [(or (null? x) (> (first (list y z)) x))
            (max-and-sum-helper 
                (rest (list y z)) 
                (first (list y z)) 
                (+ x (first (list y z)))
            )
        ]
        [else (max-and-sum-helper (rest (list y z)) x (+ x (first (list y z))))]
    )
)

; 

(second (cond
        [#f (list x x)]
        [(or #f (> y x))
            (max-and-sum-helper 
                (list z)                    ; lst = (list z)
                y                           ; mxs = (cond [(> y x) (y)] [else x])
                (+ x y)                     ; buff = (+ x y)
            )
        ]
        [else 
            (max-and-sum-helper 
                (list z)
                x 
                (+ x y)
            )
        ]
    )
)

;

(second (cond
        [#f (list (list) (cond [(> y x) y] [else x]) (+ x y))]
        [(or #f (> z (cond [(> y x) y] [else x]))) 
            (max-and-sum-helper 
                (list)                              ; lst = (list)
                z                                   ; mxs = (cond [(> z (cond [(> y x) (y)] [else x])) z] [else (cond [(> y x) (y)] [else x])])
                (+ z (+ x y))                       ; buff = (+ x y z)
            )
        ]
        [else 
            (max-and-sum-helper 
                (list)
                (cond [(> y x) y] [else x])
                (+ z (+ x y))   
            )
        ]
    )
)

; 

(second
    (cond
        [(empty? (list)) (list (cond [(> z (cond [(> y x) (y)] [else x])) z] [else (cond [(> y x) (y)] [else x])]) (+ x y z))]
        [...]
        [...]
    )
)

;

(second (list 
    (cond [(> z (cond [(> y x) (y)] [else x])) z] [else (cond [(> y x) (y)] [else x])]) 
    (+ x y z)
))

;

(+ x y z)