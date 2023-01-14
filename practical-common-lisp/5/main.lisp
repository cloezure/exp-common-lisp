;;(defun name (parameter*)
;;  "Optional documentation string."
;;  body-form*)

(defun verbose-sum (x y)
  "Sum any two numbers after printing a message."
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))

(defun foo (a b &optional c d) (list a b c d))

;;(foo 1 2)     ==> (1 2 NIL NIL)
;;(foo 1 2 3)   ==> (1 2 3 NIL)
;;(foo 1 2 3 4) ==> (1 2 3 4)

;;(foo 1 2) ==> (1 2)
;;(foo 1)   ==> (1 10)

;;(defun make-rectangle (width &optional (height width)) ...)


(defun foo (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))

;;; This gives results like this:

;;(foo 1 2)   ==> (1 2 3 NIL)
;;(foo 1 2 3) ==> (1 2 3 T)
;;(foo 1 2 4) ==> (1 2 4 T)

;;; REST

;;(defun format (stream string &rest values) ...)
;;(defun + (&rest numbers) ...) 


;;; KEYWORD

;;(defun foo (&key a b c) (list a b c))

(defun foo (&key (a 0) (b 0 b-supplied-p) (c (+ a b)))
  (list a b c b-supplied-p))

;;(foo :a 1) ==> (1 0 1 NIL)
;;(foo :b 1) ==> (0 1 1 T)
;;(foo :b 1 :c 4) ==> (0 1 4 T)
;;(foo :a 2 :b 1 :c 4) ==> (2 1 4 T)

;;(defun foo (&key ((:apple a)) ((:box b) 0) ((:charlie c) 0 c-supplied-p))
;;  (list a b c c-supplied-p))

;(foo :apple 10 :box 20 :charlie 30) ==> (10 20 30 T)

(defun foo (&rest rest &key a b c) (list rest a b c))

;;(foo :a 1 :b 2 :c 3)  ==> ((:A 1 :B 2 :C 3) 1 2 3)

(defun foo (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from foo (list i j))))))

