#lang sicp

(define (test label pred res)
    (if (= pred res)
        (string-append "OK: " label)
        (begin
          (display pred)
          (display " Not equal to ")
          (display res)
          (string-append " ERROR: " label))))

;; Ex 1.3: Define a procedure that takes three numbers
;; as arguments and returns the sum of the squares of
;; the two larger numbers. 

(define (square x) (* x x))
(define (sum-squares x y) (+ (square  x) (square y)))
(define (sum-squares-2-largest x y z)
  (cond ((and (<= x y) (<= x z)) (sum-squares y z))
        ((and (<= y x) (<= y z)) (sum-squares x z))
        ((and (<= z x) (<= z y)) (sum-squares x y))))

(test "Sum 2 largest squares 1 2 1"
      (sum-squares-2-largest 1 2 1)
      5)
(test "Sum 2 largest squares 1 1 2"
      (sum-squares-2-largest 1 1 2)
      5)
(test "Sum 2 largest squares 2 1 1"
      (sum-squares-2-largest 2 1 1)
      5)

(test "Sum 2 largest squares 1 1 1"
      (sum-squares-2-largest 1 1 1)
      2)
;; Square Roots by Newton's Method:
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; Usinf new-if instead of if in the sqrt-iter definition leads
;; infinite loop evaluation du to the eager evaluation of function
;; arguments, in particular the recursive call.
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; Alternatif  good-enough implementation
(define (sqrt-alt x)
  (sqrt-iter-alt 1.0 x))

(define (sqrt-iter-alt guess x)
  (if (good-enough?-alt guess x)
      guess
      (sqrt-iter-alt (improve guess x) x)))

(define (good-enough?-alt guess x)
  (let ((sq-guess (square guess)))
    (< (/ (abs (- sq-guess x)) x) 0.0000001)))

;; Implement cube root procedure
(define (cube-root x)
  (cube-root-iter 1.0 x))

(define (cube-root-iter guess x)
  (if (cube-good-enough? guess x)
      guess
      (cube-root-iter (cube-improve guess x)
                      x)))

(define (cube-good-enough? guess x)
  (< (/ (abs (- (cube guess) x)) x) 0.00001))

(define (cube x)
  (* x x x))

(define (cube-improve guess x)
  (/ (+ (/ x
           (square guess))
        (* 2 guess))
     3))