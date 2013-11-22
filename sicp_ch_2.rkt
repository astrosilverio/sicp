#lang scheme

(define (average x y)
  (/ (+ x y) 2))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-segment p q)
  (cons p q))

(define (start-segment l)
  (car l))

(define (end-segment l)
  (cdr l))

(define (print-segment l)
  (newline)
  (print-point (start-segment l))
  (display " ->")
  (print-point (end-segment l))
  (newline))

(define (midpoint-segment l)
  (make-point (average (x-point (start-segment l)) (x-point (end-segment l)))
              (average (y-point (start-segment l)) (y-point (end-segment l)))))

(define point-one (make-point 1 1))
(define point-two (make-point 1 5))
(define line (make-segment point-one point-two))
(print-segment line)
(print-point (midpoint-segment line))

(define (cons-two x y)
  (lambda (m) (m x y)))

(define (car-two z)
  (z (lambda (p q) p)))

(define (cdr-two z)
  (z (lambda (p q) q)))

(car-two (cons-two 5 2))
(cdr-two (cons-two 5 4))

(define (cons-three a b)
  (* (expt 2 a) (expt 3 b)))

(define (car-three z)
  (define (divide-two-iter x it) (if (= (remainder x 2) 0) (divide-two-iter (/ x 2) (+ it 1))
                                  it))
  (divide-two-iter z 0))

(define (cdr-three z)
  (define (divide-three-iter x it) (if (= (remainder x 3) 0) (divide-three-iter (/ x 3) (+ it 1))
                                  it))
  (divide-three-iter z 0))


(car-three (cons-three 3 4))
(cdr-three (cons-three 3 4))

(define zero (lambda (f) (lambda (x) x)))

(define (inc x) (+ x 1))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define church-one (lambda (f) (lambda (x) (f x))))
(define church-two (lambda (f) (lambda (x) (f (f x)))))
(define (church-add a b) (lambda (f) (lambda (x) ((a f) (b f) x))))

((church-two inc) 4)

;; prob 2.7, 2.8

(define (make-interval a b)
  (cons a b))

(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (mul-two x y)
  (cond ((and (> (lower-bound x) 0) (> (lower-bound y) 0)) (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
        ((and (< (upper-bound x) 0) (< (upper-bound y) 0)) (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
        ((or (and (> (lower-bound x) 0) (< (upper-bound y) 0)) (and (< (upper-bound x) 0) (> (lower-bound y) 0))) (let ((p1 (* (lower-bound x) (upper-bound y)))
                                                                                                                       (p2 (* (upper-bound x) (lower-bound y))))
                                                                                                                   (make-interval (min p1 p2) (max p1 p2))))
        ((and (> (lower-bound x) 0) (and (< (lower-bound y) 0) (> (upper-bound y) 0))) (make-interval (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
        ((and (> (lower-bound y) 0) (and (< (lower-bound x) 0) (> (upper-bound x) 0))) (make-interval (* (upper-bound y) (lower-bound x)) (* (upper-bound y) (upper-bound x))))
        ((and (< (upper-bound y) 0) (and (< (lower-bound x) 0) (> (upper-bound x) 0))) (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound y) (lower-bound x))))
        ((and (< (upper-bound x) 0) (and (< (lower-bound y) 0) (> (upper-bound y) 0))) (make-interval (* (upper-bound y) (lower-bound x)) (* (lower-bound y) (lower-bound x))))        
        ((and (> (* (upper-bound x) (upper-bound y)) 0) (> (* (lower-bound x) (lower-bound y)) 0)) (mul-interval x y))))
                                                                                                                   
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (sub-two x y)
  (let ((p1 (- (lower-bound x) (lower-bound y)))
        (p2 (- (lower-bound x) (upper-bound y)))
        (p3 (- (upper-bound x) (lower-bound y)))
        (p4 (- (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))


(define x (make-interval 4 7))
(define y (make-interval -2 10))
(define z (make-interval 1 13))

(+ (width x) (width y))

(width (add-interval x y))
(width (sub-interval x y))
(width (mul-interval x y))
(width (mul-interval x z))
(width (div-interval x y))
(width (div-interval x z))

(define (int-sort x y)
  (if (or (and (> (lower-bound y) 0) (< (upper-bound x) 0)) (and (> (lower-bound x) 0) (< (upper-bound x) 0)))
      (cons y x)
      (cons x y)))

(define (mul-better x y)
  (let ((a (car (int-sort x y)))
        (b (cons (car (cdr (int-sort x y))) (cdr (cdr (int-sort x y))))))
    (cond ((and (> (lower-bound a) 0) (> (lower-bound b) 0)) (make-interval (* (lower-bound a) (lower-bound b)) (* (upper-bound a) (upper-bound b))))
          ((and (< (upper-bound a) 0) (< (upper-bound b) 0)) (make-interval (* (upper-bound a) (upper-bound b)) (* (lower-bound a) (lower-bound b))))
          ((and (> (lower-bound a) 0) (< (upper-bound b) 0)) (make-interval (* (upper-bound a) (lower-bound b)) (* (lower-bound a) (upper-bound b))))
          ((and (> (lower-bound a) 0) (and (< (lower-bound b) 0) (> (upper-bound b) 0))) (make-interval (* (upper-bound a) (lower-bound b)) (* (upper-bound a) (* upper-bound b))))
          ((and (< (upper-bound a) 0) (and (< (lower-bound b) 0) (> (upper-bound b) 0))) (make-interval (* (lower-bound a) (upper-bound b)) (* (lower-bound a) (* lower-bound b))))
          ((and (> (* (upper-bound x) (upper-bound y)) 0) (> (* (lower-bound x) (lower-bound y)) 0)) (mul-interval x y)))))

(int-sort x y)
(car (int-sort x y))
(car (cdr (int-sort x y)))
(cdr (cdr (int-sort x y)))
(cons (car(cdr (int-sort x y))) (cdr(cdr(int-sort x y))))
(mul-better x z)
(mul-better y z)

(define (make-center-percent c p)
  (make-interval (- c (* c p 0.01)) (+ c (* c p 0.01))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (- 1 (/ (lower-bound i) (center i))))

(define i (make-center-percent 10 0.055))

(define (mul-small-error x y)
  (let ((c1 (center x))
        (c2 (center y))
        (p1 (percent x))
        (p2 (percent y)))
    (make-center-percent (* c1 c2) (+ p1 p2))))

(define a (make-center-percent 10 0.001))
(define b (make-center-percent 15 0.001))
(mul-interval a b)
(mul-small-error a b)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (define (length-iter a count)
    (if (not (pair? a))
        (+ 1 count)
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (last-pair items)
  (list-ref items (- (length items) 1)))

(define (last items)
  (if (= (length items) 2)
      (cdr items)
      (last (cdr items))))

(define (flatten x)
    (cond ((null? x) '())
          ((not (pair? x)) (list x))
          (else (append (flatten (car x))
                        (flatten (cdr x))))))

(define (reverse items)
  (if (null? items)
      '()
      (if (not (pair? items))
          items
          (append (reverse (cdr items)) (list (car items))))))
      
       
(reverse (list 1 2 3 4))

;(last-pair (list 1 2 3 4))
(last (list 1 2 3 4))

(define (first-denomination kinds-of-coins)
  (car kinds-of-coins))

(define (no-more? kinds-of-coins)
  (null? kinds-of-coins))

(define (except-first-denomination kinds-of-coins)
  (car (cdr kinds-of-coins)))

(define (even? n)
  (= 0 (remainder n 2)))


 
(flatten '((1) 2 ((3 4) 5) ((())) (((6))) 7 8 ()))

(define (same-parity x . y)
  (define (filter-iter n list acc)
    (if (null? list)
        (reverse acc)
        (if (even? (+ n (car list))) (filter-iter n (cdr list) (cons (car list) acc))
            (filter-iter n (cdr list) acc))))
  (filter-iter x y (list x)))
  
 
(even? 16)
(even? 25)

(same-parity 6 1 2 3 4 5 6 7 8)

;(define (map proc items)
 ; (if (null? items)
 ;     '()
  ;    (cons (proc (car items)) (map proc (cdr items)))))

(map (lambda (x) (* x x)) (list 1 2 3 4 5))

(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list-map items)
  (map (lambda (x) (* x x)) items))

(square-list (list 1 2 3 4 5))
(square-list-map (list 1 2 3 4 5))

(define (square x)
  (* x x))

(define (square-list-iter-backwards items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things)) answer))))
  (iter items '()))

(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer (square (car things))))))
  (iter items '()))

(define dotted (cons 3 4))
;(cdr (cdr dotted))
(cdr (cdr (list 3 4)))
    

(square-list-iter-backwards (list 1 2 3 4 5))
(square-list-iter (list 1 2 3 4 5))

(define (for-each proc list)
  (if (null? (cdr list))
      (proc (car list))
      (and (proc (car list)) (for-each proc (cdr list)))))

(define (for-each-two proc list)
  (cond ((not (null? list)) (proc (car list))
                            (for-each-two proc (cdr list)))))

(for-each-two (lambda (x) (newline) (display x)) (list 35 98 447))
    
(list 1 (list 2 (list 3 4)))

(define twotwofiveone (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr twotwofiveone)))))
(define twotwofivetwo (list (list 7)))
(car (car twotwofivetwo))
(define twotwofivethree (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr twotwofivethree))))))))))))

(define sixx (list 1 2 3))
(define sixy (list 4 5 6))

(append sixx sixy)
(cons sixx sixy)
(list sixx sixy)

(define (deep-reverse items)
  (if (null? (cdr items))
      (list (reverse (car items)))
      (if (pair? (car items))
          (append (deep-reverse (cdr items)) (list (deep-reverse (car items))))
          (append (deep-reverse (cdr items)) (list (car items))))))


(define test-list (list (list 1 2) (list 3 4)))
(deep-reverse (list (list 1 2) (list 3 4) 5))
test-list
(reverse test-list)
(deep-reverse test-list)

(define (fringe items)
  (if (null? items)
      '()
      (if (not (pair? items))
          (list items)
          (append (fringe (car items)) (fringe (cdr items))))))

(fringe (list test-list test-list))

(define (left-branch struct)
  (car struct))
(define (right-branch struct)
  (car (cdr struct)))
(define (branch-length struct)
  (car struct))
(define (branch-struct struct)
  (car (cdr struct)))
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define test-mobile (make-mobile (make-branch 10 (make-mobile (make-branch 10 10) (make-branch 10 3))) (make-branch 4 50)))

(define (branch-weight branch)
  (if (pair? (branch-struct branch))
      (total-weight (branch-struct branch))
      (branch-struct branch)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(total-weight test-mobile)

(define (torque branch)
  (* (branch-length branch) (total-weight branch)))

(define (balanced? mobile)
  (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
      true
      false))

(define (square-tree  tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (map-square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (map-square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

(define (tree-map proc tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree)) (tree-map proc (cdr tree))))))

(map-square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(tree-map square (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (acc-map proc sequence)
  (accumulate (lambda (x y) (cons (proc x) y)) '() sequence))

(define (acc-app seq1 seq2)
  (accumulate cons seq2 seq1))

(define (acc-length sequence)
  (accumulate (lambda (x y) (+ (length x) y)) 0 sequence))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* x higher-terms) this-coeff)) 0 coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

(count-leaves (list 1 2 3 (list 4 5 6 (list 7 8) 9)))

(count-leaves (list 1 2 3 4 5 (list 2 3 4) 2 3 4))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 10 s)

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (y) (dot-product v y)) m))

(define (transpose m)
  (accumulate-n cons '() m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (rows) (matrix-*-vector cols rows)) m)))

(define mat (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define v (list 1 2 3 1))

(matrix-*-vector mat v)
(transpose mat)

(define mat2 (list (list 1 0 0 ) (list 0 2 0 ) (list  0 3 0) (list 0 0 4)))

(matrix-*-matrix mat (transpose mat))
(matrix-*-matrix mat2 mat)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(accumulate + 1 (list 1 2 3))
(fold-left + 1 (list 1 2 3))
(accumulate list '() (list 1 2 3))
(fold-left list '() (list 1 2 3))

(define (rev-right sequence)
  (accumulate (lambda (x y) (append y (list x))) '() sequence))
(rev-right (list 1 2 3 4 5))

(define (rev-left sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

(rev-left (list 1 2 3 4 5))