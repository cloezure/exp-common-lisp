;;; Numbers
;; 123       ; the integer one hundred twenty-three
;; 3/7       ; the ratio three-sevenths
;; 1.0       ; the floating-point number one in default precision
;; 1.0e0     ; another way to write the same floating-point number
;; 1.0d0     ; the floating-point number one in "double" precision
;; 1.0e-4    ; the floating-point equivalent to one-ten-thousandth
;; +42       ; the integer forty-two
;; -42       ; the integer negative forty-two
;; -1/4      ; the ratio negative one-quarter
;; -2/8      ; another way to write negative one-quarter
;; 246/2     ; another way to write the integer one hundred twenty-three

;;; Strings literals
;; "foo"     ; the string containing the characters f, o, and o.
;; "fo\o"    ; the same string
;; "fo\\o"   ; the string containing the characters f, o, \, and o.
;; "fo\"o"   ; the string containing the characters f, o, ", and o.

;;; Symbols
;; x             ; the symbol X
;; ()            ; the empty list
;; (1 2 3)       ; a list of three numbers
;; ("foo" "bar") ; a list of two strings
;; (x y z)       ; a list of three symbols
;; (x 1 "foo")   ; a list of a symbol, a number, and a string
;; (+ (* 2 3) 4) ; a list of a symbol, a list, and a number.


;;; Function Calls
;; (function-name argument*)
;; (+ 1 2)

;;; Special Operators
;; (if x (format t "yes") (format t "no"))
;; (if test-form then-form [ else-form ])

(defvar a 'a)
(defmacro ab (x y z) 42)

(print (ab pof rif paf))


