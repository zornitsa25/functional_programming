#lang racket

(define (approx-zero? x eps)
  (and (< x eps) (> x 0)))

(define (find-root f a b eps)
  (define (helper f left right mid i)
    (cond((approx-zero? (abs(f mid)) eps) (cons (real->double-flonum mid) i))
         ((< (f mid) 0) (helper f mid right (/ (+ mid right) 2) (+ i 1)))
         (else (helper f left mid (/ (+ mid left) 2) (+ i 1)))))
  (helper f a b (/ (+ a b) 2) 0))

; (real->double-flonum x) - convert rational number to floating point

(define (derive f dx)
  (lambda (x)
    (/ (- (f (+ x dx)) (f x)) dx)))

; dx - small number (approximately zero)

(define (der f x)
  ((derive f 1e-10) x))

(define (formulaNewton f c)
  ((lambda (x) (- x (/ (f c) (der f x)))) c))

(define (find-root-Newton f a b eps)
  (define x1 (formulaNewton f b))
  (define x2 (formulaNewton f x1))
  (define (helper xi nextxi eps i)
    (if (< (abs(- xi nextxi)) eps)
        (cons nextxi i)
        (helper nextxi (formulaNewton f nextxi) eps (+ i 1))))
  (helper x1 x2 eps 0))

; (find-root-Newton (lambda (x) (- (sqrt x) (* 2 (log x)))) 1 5 0.01) - doesn't work
; x - double -> log(x) becomes negative -> sqrt(x) is imaginary number

(define (compare-methods f a b eps)
  (list (find-root f a b eps) (find-root-Newton f a b eps)))

(define (compare-methods-iters f a b eps)
  (map cdr (list (find-root f a b eps) (find-root-Newton f a b eps))))