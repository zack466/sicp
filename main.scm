(load "sicp.scm")
(load "chapter2.scm")

; (display (deriv '(* x y (+ x 3)) 'x))
; (newline)

(display (make-sum 'a 'b 'c))
(newline)
(display (addend (make-sum 'a 'b 'c)))
(newline)
(display (augend (make-sum 'a 'b 'c)))
(newline)

