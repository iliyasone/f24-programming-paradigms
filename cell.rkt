#lang slideshow

(define (my-or lst)
    (cond 
        [(empty? lst) #f]
        [(first lst) #t]
        [else (my-or (rest lst))]
    )
)

(define (my-ormap f lst)
    (my-or (map f lst))
)

(define (insert srt el acc)
   (cond
    [(and (empty? srt) (null? el)) acc]
    [(empty? srt) (cons el acc)]
    [(> el (first srt)) (insert (rest srt) el (cons (first srt) acc))]
    [else (insert (rest srt) null (cons (first srt)))]
   )
)

(define (insert-sort srt rst)
    (cond 
        [(empty? rst) srt]
        [(empty? srt) (insert-sort (first rst) (rest rst))]
        []
    )
)