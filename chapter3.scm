; balance is internal to withdraw
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))))

; can create multiple withdraw objects
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "insufficient funds")))

; now with dispath for more behavior(define (make-account balance)
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request: MAKE-ACCOUNT"
		       m))))
  dispatch)

; exercise 3.1
(define (make-accumulator initial)
  (lambda (arg)
    (begin (set! initial (+ initial arg))
	   initial)))

; exercise 3.2
(define (make-monitored f)
  (define calls 0)
  (lambda (input)
    (cond ((eq? input 'how-many-calls) calls)
	  ((eq? input 'reset-count)
	   (begin (set! calls 0) 0))
	  (else (begin (set! calls (+ calls 1))
		       (f input))))))

; exercise 3.8
; (+ (f 0) (f 1)) -> 0
; (+ (f 1) (f 0)) -> 1
(define (make-flip other)
  (define flag 0)
  (lambda (input)
    (if (= (modulo flag 2) 0)
      (begin (set! flag (inc flag)) input)
      (begin (set! flag (inc flag)) other))))
(define f (make-flip 0))


