#lang racket

(define (getLastDigit n)
  (remainder n 10))

(define (getSecondLastDigit n)
  (getLastDigit (quotient n 10)))

(define (maxDigit n)
  (define (helper maxNum num)
    (define next (getSecondLastDigit num))
    (cond ((< num 10) maxNum)
          ((>= next maxNum) (helper next (quotient num 10)))
          (else (helper maxNum (quotient num 10)))))
  (helper (getLastDigit n) n))

(define (getIndexPow n)
  (define (helper i temp maxNum num)
    (define next (getLastDigit num))
    (cond ((< num 1) i)
          ((= maxNum next) (helper temp (+ temp 1) maxNum (quotient num 10)))
          (else (helper i (+ temp 1) maxNum (quotient num 10)))))
  (helper 0 0 (maxDigit n) n))

(define (pow n i)
  (cond((= i 0) 1)
       ((< i 0) (pow (/ 1 n) (- i (* 2 i))))
       ((even? i) (pow (* n n) (/ i 2)))
       (else (* n (pow (* n n) (/ (- i 1) 2))))))

(define (removeMaxNum n)
  ((lambda (a b) (+ a (* b (pow 10 (getIndexPow n)))))
    (remainder n (pow 10 (getIndexPow n)))
    (quotient n (pow 10 (+ (getIndexPow n) 1)))))

(define (reduce n)
  (if(< n 10)
     n
     (reduce (* (removeMaxNum n) (maxDigit n)))))