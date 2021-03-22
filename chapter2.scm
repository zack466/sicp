(load "sicp.scm")

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; (define make-rat cons)
; (define numer car)
; (define denom cdr)
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

; exercise 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((n (/ n g)) (d (/ d g)))
      (cond
        ((and (< n 0) (> d 0))
         (cons n d))
        ((and (> n 0) (< d 0))
         (cons (- n) (- d)))
        (else
          (cons (abs n) (abs d)))))))

; exercise 2.5
; cons = make-pair
; car = first
; cdr = second
(define (make-pair a b) (* (fast-expt 2 a) (fast-expt 3 b)))

; gives largest power of div which divides num
(define (maxdiv num div counter)
  (if (= 0 (remainder num div))
      (maxdiv (/ num div) div (inc counter))
      counter))

(define (first ab)
  (maxdiv ab 2 0))
(define (second ab)
  (maxdiv ab 3 0))


; interval representation
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

(define (div-interval x y)
  (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))

; exercise 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

; exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


; exercise 2.10
(define (div-interval x y)
  (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
      (error "Dividend cannot include zero")
      (mul-interval
        x
        (make-interval (/ 1.0 (upper-bound y))
                       (/ 1.0 (lower-bound y))))))

; exercise 2.11
(define (mul-interval x y)
  (define (neg? x) (< x 0))
  (define (pos? x) (> x 0))
  (let ((a (lower-bound x))
        (b (upper-bound x))
        (c (lower-bound y))
        (d (upper-bound y)))
    (cond
      ((pos? a) (cond
                  ((pos? c)
                   (make-interval (* a c) (* b d)))
                  ((pos? d)
                   (make-interval (* b c) (* b d)))
                  (else
                    (make-interval (* b c) (* a d)))))
      ((pos? b) (cond
                  ((pos? c)
                   (make-interval (* a d) (* b d)))
                  ((pos? d)
                   (make-interval
                     (min (* b c) (* a d))
                     (max (* a c) (* b d))))
                  (else
                    (make-interval (* b c) (* a c)))))
      (else (cond
              ((pos? c)
               (make-interval (* a d) (* b c)))
              ((pos? d)
               (make-interval (* a d) (* a c)))
              (else
                (make-interval (* b d) (* a c))))))))


(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; exercise 2.12
(define (make-center-percent c p)
  (let ((w (* c (/ p 100.0))))
    (make-center-width c w)))
(define (percent i)
  (* 100.0 (/ (width i) (center i))))


; lists
(define nil '())

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

; exercise 2.17
(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

; exercise 2.18
(define (reverse l)
  (define (iter arr result)
    (if (null? arr)
        result
        (iter (cdr arr) (cons (car arr) result))))
  (iter l nil))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

; exercise 2.21
(define (square-list list1)
  (map (lambda (x) (* x x)) list1))

; exercise 2.23
(define (for-each f ls)
  (if (null? ls)
      nil
      (begin
        (f (car ls))
        (for-each f (cdr ls)))))

; exercise 2.27
(define (deep-reverse l)
  (define (iter arr result)
    (if (null? arr)
        result
        (iter (cdr arr) (cons (reverse (car arr)) result))))
  (if (not (pair? l))
      l
      (iter l nil)))

; exercise 2.28
(define (fringe x)
  (define (iter x result)
    (cond
      ((null? x) result)
      ((pair? x) (append (iter (car x) result) (iter (cdr x) result)))
      (else (append result (list x)))))
  (iter x nil))

; exercise 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

(define (total-weight mobile)
  (cond
    ((null? mobile) 0)
    ((not (pair? mobile)) mobile)
    (else
      (+ (total-weight (cadr (left-branch mobile)))
         (total-weight (cadr (right-branch mobile)))))))

(define (balanced? mobile)
  (if (pair? mobile)
      (and
        (= (* (car (left-branch mobile))
              (total-weight (cadr (left-branch mobile))))
           (* (car (right-branch mobile))
              (total-weight (cadr (right-branch mobile)))))
        (balanced? (cadr (left-branch mobile)))
        (balanced? (cadr (right-branch mobile))))
      #t))

; exercise 2.31
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))

; exercise 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map
                       (lambda (x)
                         (cons (car s) x))
                       rest)))))

; (display (subsets (list 1 2 3)))


(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        (( predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (inc low) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate
    + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate
    cons
    nil
    (filter even? (map fib (enumerate-interval 0 n)))))

; exercise 2.33
(define (map- p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(define (append- seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length- sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; exercise 2.34
; coeff-seq: (a_0, a_1, a_2, ... a_n)
(define (horner-eval x coeff-seq)
  (accumulate (lambda (coeff higher-terms)
                (+ coeff (* x higher-terms)))
              0
              coeff-seq))

; exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))


(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                            (lambda (i)
                              (map (lambda (j) (list i j))
                                   (enumerate-interval 1 (- i 1))))
                            (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))



