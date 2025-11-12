
;ejercicio 1

#lang racket
(define (count-positives lst)
  (length (filter (lambda (x) (> x 0)) lst)))

(count-positives '(3 -2 7 0 -5 9))

;ejercicio 2

(define (even-squares lst)
  (filter even? (map (lambda (x) (* x x)) lst)))

(even-squares '(1 2 3 4 5 6 7 8))

;ejercicio 3

(define (factorial n)
  (if (<= n 1) 1 (* n (factorial (- n 1)))))

(factorial 5)

;ejercicio 4

(define (cube-list lst)
  (map (lambda (x) (* x x x)) lst))

(cube-list '(2 3 4))


;ejercicio 5

(define (sum-odds lst)
  (foldl + 0 (filter odd? lst)))

(sum-odds '(1 2 3 4 5 6 7))

;ejercicio 6

(define (has-negatives lst)
  (ormap (lambda (x) (< x 0)) lst))

(has-negatives '(5 9 -3 2))

;ejercicio 7

(define (cumulative-sum lst)
  (reverse
   (foldl (lambda (x acc)
            (cons (+ x (if (null? acc) 0 (car acc))) acc))
          '()
          lst)))

(cumulative-sum '(1 2 3 4))

;ejercicio 8

(define (concat-strings lst)
  (foldl (lambda (x acc) (string-append acc x)) "" lst))

(concat-strings '("Hola" " " "Mundo"))

;ejercicio 9

(define (double-greater-than-five lst)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) lst)))

(double-greater-than-five '(3 6 8 2 10))

;ejercicio 10

(define (reverse-list lst)
  (foldl (lambda (x acc) (cons x acc)) '() lst))

(reverse-list '(1 2 3 4))

;ejercicio 11

(define (apply-func f lst)
  (map f lst))

(define (cuadrado x) (* x x))

(apply-func cuadrado '(1 2 3 4))


;ejercicio 12

(define (average-greater-than-five lst)
  (let* ((filtered (filter (lambda (x) (> x 5)) lst))
         (sum (foldl + 0 filtered))
         (count (length filtered)))
    (exact->inexact (/ sum count))))

(average-greater-than-five '(3 8 10 4 9 2 7))
