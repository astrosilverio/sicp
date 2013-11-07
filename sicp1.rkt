#lang scheme

(define (square x) (* x x))
(define (cube x) (* x x x))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00001)

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (new-sqrt x)
  (newtons-method (lambda(y) (- (square y) x))
                  1.0))
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x)
  (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (lambda (x) ((compose f (repeated f (- n 1))) x))))

(define (root x n damps)
  (fixed-point ((repeated average-damp damps) (lambda (y) (/ x (expt y (- n 1))))) 1.0))

(define (better-root x n)
  (fixed-point ((repeated average-damp (floor (/ (log n) (log 2)))) (lambda (y) (/ x (expt y (- n 1))))) 1.0))

(expt (root 100 121 6) 121)
(expt (better-root 100 500) 500)

