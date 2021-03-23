; abstract data / generic operations

; selectors
; real-comp, imag-comp, magnitude, angle

(define (add-complex z1 z2)
  (from-components (+ (real z1) (real z2))
		   (+ (imag z1) (imag z2))))

(define (sub-complex z1 z2)
  (from-components (- (real z1) (real z2))
		   (- (imag z1) (imag z2))))

(define (mul-complex z1 z2)
  (from-polar (* (magnitude z1) (magnitude z2))
	      (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (from-polar (/ (magnitude z1) (magnitude z2))
	      (- (angle z1) (angle z2))))

; with rectangular representation
(define (real z) (car z))
(define (imag z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z))
	   (square (imag-part z)))))
(define (angle z)
  (atan (imag z) (real z)))
(define (from-components x y) (cons x y))
(define (from-polar r a)
  (cons (* r (cos a)) (* r (sin a))))

; with polar representation
(define (real z) (* (magnitude z) (cos (angle z))))
(define (imag z) (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (from-components x y)
  (cons (sqrt (+ (square x) (square y)))
	(atan y x)))
(define (from-polar r a) (cons r a))

; type tags
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum: CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

; revised constructors/selectors
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
	   (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
	(real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
	      (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
	      (cons (sqrt (+ (square x) (square y)))
		    (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

; generic selectors (don't have to change math operators)
; all just dispatch on type to a function name
(define (real-part z)
  (cond ((rectangular? z)
	 (real-part-rectangular (contents z)))
	((polar? z)
	 (real-part-polar (contents z)))
	(else "Unknown type: REAL-PART" z)))
(define (imag-part z)
  (cond ((rectangular? z)
	 (imag-part-rectangular (contents z)))
	((polar? z)
	 (imag-part-polar (contents z)))
	(else (error "Unknown type: IMAG-PART" z))))
(define (magnitude z)
  (cond ((rectangular? z)
	 (magnitude-rectangular (contents z)))
	((polar? z)
	 (magnitude-polar (contents z)))
	(else (error "Unknown type: MAGNITUDE" z))))
(define (angle z)
  (cond ((rectangular? z)
	 (angle-rectangular (contents z)))
	((polar? z)
	 (angle-polar (contents z)))
	(else (error "Unknown type: ANGLE" z))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

; not including data table/message passing part of textbook
; bc requires code from later in the book
