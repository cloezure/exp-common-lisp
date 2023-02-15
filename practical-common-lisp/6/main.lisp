;;; лексические переменные x y z
(defun foo (x y z) (list x y z))

;;; let (variable*) body-form*)

(let ((x 10) (y 20) z))


(defun foo (x)
  (format t "Параметр: ~a~%" x)
  (let ((x 2))
    (format t "Внешний LET: ~a~%" x)
    (let ((x 3))
      (format t "Внутренний LET ~a~%" x))
    (format t "Внешний LET: ~a~%" x))
  (format t "Параметр: ~a~%" x))


(let* ((x 10)
       (y (+ x 10)))
  (list x y))

;;; замыканиие
(let ((count 0))
  #'(lambda () (incf count)))

(defparameter *fn*
  (let ((count 0))
    #'(lambda () (incf count))))


(let ((count 0))
  (list
   #'(lambda () (incf count))
   #'(lambda () (decf count))
   #'(lambda () count)))


;;; динамические переменные
(defvar *count* 0
  "Число уже созданных виджетов.")

(defparameter *gap-tolerance* 0.001
  "Допустимое отклонение интервала между виджетами.")

(defun increment-widget-count () (incf *count*))


(defvar *x* 10)
(defun foo () (format t "X: ~d~%" *x*))

(defun bar ()
  (foo)
  (let ((*x* 20)) (foo))
  (foo))

(defun foo ()
  (format t "Перед присваиванием~18tX: ~d~%" *x*)
  (incf *x*)
  (format t "После присваиванием~18tX: ~d~%" *x*))


;;; (defconstant name initial-value-form [documentation-string])
(defconstant +some+ 10
  "some constant ~_~")

;;; (setf place value)

(let ((x 42) (y 72))
  (defun info-var ()
    (format t "x: ~d~10ty: ~d~%" x y))
  (setf x 1)
  (setf y 2)
  (info-var)
  ;; or
  (setf x 213 y 42)
  (info-var)
  ;; последовательное присваивание
  (setf x (setf y (random 10)))
  (info-var))


;;; Обобщённое присваивание
;;Simple variable:    (setf x 10) 
;;Array:              (setf (aref a 0) 10)
;;Hash table:         (setf (gethash 'key hash) 10)
;;Slot named 'field': (setf (field o) 10)

;;(setf x (+ x 1) or (setf x (- x 1))
;; to (incf x) (decf x)
;; or (incf x 10) => (setf x (+ x 10))

;; (incf (aref *array* (random (length *array*))))

;;; rotatef === swap
;; (rotatef and shiftf)
;; (rotatef a b) like swap(a,b)
(let ((x 42) (y 12))
  (format t "До x: ~d~10ty: ~d~%" x y)
  (rotatef x y)
  (format t "После x: ~d~10ty: ~d~%" x y))

;;; shiftf === сдвиг значений
;; (shiftf a b 10) like some a = b b = 10
(let ((x 42) (y 12))
  (format t "До x: ~d~10ty: ~d~%" x y)
  (shiftf x y 52)
  (format t "После x: ~d~10ty: ~d~%" x y))


