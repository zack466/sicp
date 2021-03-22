(load "sicp.scm")

(define (id x) x)
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (div2 x) (/ x 2))

; (define (abs- x)
;   (if (< x 0) (- x) x))

; exercise 1.2
(define (sum-two-largest a b c)
  (cond ((and (<= a b) (<= a c)) (+ b c))
	((and (<= b a) (<= b c)) (+ a c))
	((and (<= c a) (<= c b)) (+ a b))))

; finding square root of x
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))
(define (good-enough? guess x)
  (< (abs (- (* guess guess) x)) 0.0001))
(define (improve guess x)
  (average (/ x guess) guess))
(define (average a b)
  (/ (+ a b) 2.0))
(define (sqrt- x) (sqrt-iter 1.0 x))

; exercise 1.8
(define (cbrt-iter guess x)
  (define (good-enough? guess) (< (abs (- (cube guess) x)) 0.0001))
  (define (improve guess x) (/ (+ (/ x (square guess)) (* 2.0 guess)) 3.0))
  (if (good-enough? guess)
    guess
    (cbrt-iter (improve guess x) x)))

(define (cbrt x) (cbrt-iter 1.0 x))

; sqrt with lexical scoping
(define (sqrt-- x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.0001))
  (define (improve guess) (average (/ x guess) guess))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

; factorials - recursive vs iterative

; linear recursive process
(define (fact-rec x)
  (if (= x 1)
    1
    (* x (fact-rec (- x 1))))) ; builds up chain of *s

; linear iterative process
(define (fact-iter x)
  (define (fact x acc)
    (if (= x 1)
      acc
     (fact (- x 1) (* acc x)))) ; has itself as top expression
  (fact x 1))


; exercise 1.11
(define (f-rec x)
  (if (< x 3)
    x
    (+ (f-rec (- x 1)) (* 2 (f-rec (- x 2))) (* 3 (f-rec (- x 3))))))

(define (f-iter x)
  (define (f a b c counter x)
    (define d (+ c (* 2 b) (* 3 a)))
    (if (= counter x)
      d
      (f b c d (+ 1 counter) x)))
  (if (< x 3)
    x
    (f 0 1 2 3 x)))

; exercise 1.12
(define (pascal n k)
  (cond
    ((= k 0) 1)
    ((= k n) 1)
    (else (+ (pascal (dec n) (dec k)) (pascal (dec n) k)))))

; fast exponentiation
(define (fast-expt b n)
  (cond
    ((= n 0) 1)
    ((even? n) (square (fast-expt b (/ n 2))))
    (else (* b (fast-expt b (- n 1))))))

(define (fast-expt-iter b n)
  ; want to find b^n
  ; keep ab^n invariant each iteration (starts with a=1)
  (define (expt a b n)
    (cond
      ((= n 0) a)
      ((even? n) (expt a (square b) (div2 n))) ; a -> a, b -> b^2, n -> n/2
      (else (expt (* a b) b (dec n))))) ; a -> ab, b -> b, n -> n-1
  (expt 1 b n))

; exercise 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
    ((even? count)
      (fib-iter a
        b
        (+ (square p) (square q)) ; p -> q^2 + p^2
        (+ (* 2 p q) (square q)) ; q -> 2pq + q^2
        (/ count 2)))
    (else
      (fib-iter (+ (* b q) (* a q) (* a p))
        (+ (* b p) (* a q))
        p
        q
        (- count 1)))))

; testing primality
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (expmod base exp m)
  (cond
    ((= exp 0) 1)
    ((even? exp)
      (remainder
        (square (expmod base (/ exp 2) m))
        m))
    (else
      (remainder
        (* base (expmod base (- exp 1) m))
        m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond
    ((= times 0) #t)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else #f)))

; sigma
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-cubes a b)
  (sum cube a inc b))
(define (sum-integers a b)
  (sum id a inc b))

; 1/(1*3) + 1/(5*7) + 1/(9*11) + ... approx pi/8
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.001)

; exercise 1.29
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (term x)
    (define y (f (+ a (* h x))))
    (cond
      ((or (= x 0) (= x n)) y)
      ((odd? x) (* 4.0 y))
      ((even? x) (* 2.0 y))))
  (* (/ h 3.0) (sum term 0 inc n)))

; exercise 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; exercise 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product id 1 inc n))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (approx-pi n)
  (define (numer)
    (define (term x)
      (* 2 x (+ 2 (* 2 x))))
    (product-iter term 1 inc n))
  (define (denom)
    (define (term x)
      (square (+ 1 (* 2 x))))
    (product-iter term 1 inc n))
  (* 4.0 (/ (numer) (denom))))

; exercise 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b) (accumulate + 0 term a next b))
(define (product term a next b) (accumulate * 1 term a next b))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum-iter term a next b) (accumulate-iter + 0 term a next b))
(define (product-iter term a next b) (accumulate-iter * 1 term a next b))

; exercise 1.33
(define (filtered-accumulate combiner null-value predicate term a next b)
  (cond ((> a b) null-value)
        ((not (predicate (term a))) (combiner null-value
                   (filtered-accumulate combiner null-value predicate term (next a) next b)))
        (else (combiner (term a)
                   (filtered-accumulate combiner null-value predicate term (next a) next b)))))

(define (prime? x) (fast-prime? x 5))
(define (sum-prime a b) (filtered-accumulate + 0 prime? id a inc b))

(define (coprime? x) (lambda (a) (= 1 (gcd a x))))
(define (coprime-prod n) (filtered-accumulate * 1 (coprime? n) id 1 inc n))

; half-interval method
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))
(define (close-enough? x y) (< (abs (- x y)) 0.001))
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond
      ((and (negative? a-value) (positive? b-value))
       (search f a b))
      ((and (negative? b-value) (positive? a-value))
       (search f b a))
      (else
        (error "Values are not of opposite sign" a b)))))

; (half-interval-method sin 2.0 4.0)

; finding fixed points
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; (fixed-point cos 1.0)

; fixed point of y = x/y will give sqrt of x
; however, this doens't converge, so look for average (damping)
; of y and x/y (y -> 0.5(y = x/y))
(define (sqrt-fixed x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

; exercise 1.35
(define phi
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

; exercise 1.36
(define (fixed-point-verbose f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; find x^x = 1000
; x -> log(1000) / log(x)
; (fixed-point-verbose (lambda (x) (/ (log 1000) (log x))) 10)
; 4.555532257

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt-fixed- x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

; Newton's method
; if g(x) is differentiable, then
; solutions to g(x) = 0 are fixed points of
; x -> x - g'(x)
(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt-newton x) ; zero of y^2 - x
  (newtons-method
    (lambda (y) (- (square y) x)) 1.0))

; generalizing these procedures
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-fixed-- x)
  (fixed-point-of-transform
    (lambda (y) (/ x y)) average-damp 1.0))
(define (sqrt-fixed-newton x)
  (fixed-point-of-transform
    (lambda (y) (- (square y) x)) newton-transform 1.0))

; exercise 1.41
(define (apply-twice f) (lambda (x) (f (f x))))
; (((apply-twice (apply-twice apply-twice)) inc) 5)

; exercise 1.42
(define (compose f g) (lambda (x) (f (g x))))
; ((compose square inc) 6)

; exercise 1.43
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (dec n)))))
; ((repeated square 3) 7)

; exercise 1.44
(define (smoothed f)
  (define dx 0.01)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (nth-smoothed f n)
  ((repeated smoothed n) f))

; exercise 1.45
; calculates nth root of x
(define (root n x)
  ; y -> x / y^(n-1)
  (define (fixed y) (/ x (fast-expt y (- n 1))))
  ; after damping k times, n < 2^k will converge to fixed point
  (define k (floor (/ (log n) (log 2))))
  (fixed-point ((repeated average-damp k) fixed) 1.0))

