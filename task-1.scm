#lang racket

(define (max lst)
  (cond((null? lst) '())
       ((null? (cdr lst)) (car lst))
       (else (if(< (car lst) (max (cdr lst)))
                (max (cdr lst))
                (car lst)))))

(define (min lst)
  (cond((null? lst) '())
       ((null? (cdr lst)) (car lst))
       (else (if(> (car lst) (min (cdr lst)))
                (min (cdr lst))
                (car lst)))))

(define (foldr op nv lst)
  (if (null? lst)
      nv
      (op (car lst) (foldr op nv (cdr lst)))))

(define (map f lst)
  (foldr (lambda (x y) (cons (f x) y)) '() lst))

(define (argmax f lst)
  (define (helper maxElem lst)
    (if(eq? maxElem (f (car lst)))
       (car lst)
       (helper maxElem (cdr lst))))
  (helper (max (map f lst)) lst))

(define (argmin f lst)
  (define (helper minElem lst)
    (if(eq? minElem (f (car lst)))
       (car lst)
       (helper minElem (cdr lst))))
  (helper (min (map f lst)) lst))