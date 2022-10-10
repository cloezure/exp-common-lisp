;gnu clisp  2.49.60

; lesson 14

(defun sum-list (lst &optional (acc 0))
  (cond ((null lst) acc)
	(t (sum-list (cdr lst) (+ acc (car lst))))))

(defun sum-list-r (lst)
    (cond
        ((null lst) 0)
        ((atom lst) lst)
        (t (+ (sum-list-r (car lst)) (sum-list-r (cdr lst))))))

(defun dec (num)
    (- num 1))

(defun inc (num)
    (+ num 1))

(defun what (inc)
    (inc inc))

(defun tail (x)
    (cond ((null (cdr x)) (car x))
          (t (tail (cdr x)))))

(defun range-> (max &key (min 0) (step 1))
  (cond ((<= max min) (list max))
	(t (cons max (range-> (- max step) :min min :step step)))))

(defun range-< (max &key (min 0) (step 1))
  (labels ((range-< (acc)
	  (cond ((>= acc max) (list acc))
	  (t (cons acc (range-< (+ acc step)))))))
    (range-< min)))

(defun range ((from 0) &key (to 0) (step 1))
    (cond ((<= from to) (range-< to :min from :step step))
          (t (range-> from :min to :step step))))

(defun include? (el lst)
    (cond ((null lst) nil)
          ((eq el (car lst)) t)
          (t (include? el (cdr lst)))))

(defun remove-el (el lst)
    (cond ((null lst) nil)
        ((eq el (car lst)) (remove-el el (cdr lst)))
        (t (cons (car lst) (remove-el el (cdr lst))))))

(defun set-of (lst)
    (cond ((null lst) nil)
        (t (cons (car lst) (set-of (remove-el (car lst) (cdr lst)))))))

(defun unio (s1 s2)
    (set-of (append s1 s2)))

(defun intsec (s1 s2)
    (cond 
        ((null s1) nil)
        ((include? (car s1) s2) (cons (car s1) (intsec (cdr s1) s2)))
        (t (intsec (cdr s1) s2))))

(defun diff (s1 s2)
  (cond ((null s1) nil)
	((include? (car s1) s2) (diff (cdr s1) s2))
	(t (cons (car s1) (diff (cdr s1) s2)))))

(defun sim-diff (s1 s2)
  (unio (diff s1 s2) (diff s2 s1)))

(defun eq-set (s1 s2)
  (cond ((null s1) t)
	((include? (car s1) s2) (eq-set (cdr s1) s2))
	(t nil)))

(defun eq-set2 (s1 s2)
  (null (sim-diff s1 s2)))

(defun mkpair (a lst)
  (cond ((null lst) nil)
	(t (cons (list a (car lst)) (mkpair a (cdr lst))))))

(defun decart (s1 s2)
  (cond ((null s1) nil)
	(t (append (mkpair (car s1) s2) (decart (cdr s1) s2)))))

(defun add-elt (a lsts)
  (cond ((null lsts) nil)
	(t (cons (cons a (car lsts)) (add-elt a (cdr lsts))))))

(defun all-subsets (s)
  (cond ((null s) (list nil))
	(t (let ((ss (all-subsets (cdr s))))
            (append ss (add-elt (car s) ss))))))

(defun zip (l1 l2)
  (cond ((or (null l1) (null l2)) nil)
	(t (cons (car l1) (cons (car l2) (zip (cdr l1) (cdr l2)))))))

(defun try-let ()
    (let ((result ()))
         (let ((a 10) (b 15))
             (push (+ a b) result))
         (let* ((a 10) (b (+ a 15)))
             (push (+ a b) result))
         result))
    
(defun try-dolist (lst)
    (let ((sum 0))
        (dolist (i lst sum)
            (setq sum (+ i sum)))))

(defun try-dotimes (n)
    (let ((sum 0))
        (dotimes (i n sum)
            (setq sum (+ i sum)))))

(defun try-loop (lim)
    (let ((i 1))
         (loop
          (cond ((> i lim) (return i))
                (t (setq i (inc i)))))))

(defun try-if (n)
    (if (= 0 (mod n 2))
        'even
        'odd))
    
(defun try-when (cnd)
    (when cnd
        '(1 2 3)))
    
(defun try-unless (cnd)
    (unless cnd
        '(1 2 3)))

(defun iin-lst (num lst)
    (cond
        ((null lst) (list num))
        ((> num (car lst)) (cons (car lst) (iin-lst num (cdr lst))))
        (t (cons num lst))))

(defun insert-sort (lst &optional (r nil))
    (if (null lst) r
        (insert-sort (cdr lst) (iin-lst (car lst) r))))

(defun split (d lst &optional (l nil) (r nil))
    (cond
        ((null lst) (list l r))
        ((<= (car lst) d) (split d (cdr lst) (cons (car lst) l) r))
        (t (split d (cdr lst) l (cons (car lst) r)))))

(defun quick-sort (lst)
    (if (null lst) nil
        (let ((tmp (split (car lst) (cdr lst))))
             (append
              (quick-sort (car tmp))
              (list (car lst))
              (quick-sort (cadr tmp))))))

(defun horner (p x &optional (acc 0))
    (if (null p) acc
        (horner (cdr p) x (+ (* acc x) (car p)))))

(defun add-to-tree (num tree)
    (cond 
        ((null tree) (list nil num nil))
        ((> num (cadr tree)) 
         (list (car tree) (cadr tree) (add-to-tree num (caddr tree))))
        (t (list (add-to-tree num (car tree)) (cadr tree) (caddr tree)))))

(defun list-to-tree (lst &optional (tree nil))
    (if (null lst) tree
        (list-to-tree (cdr lst) (add-to-tree (car lst) tree))))

(defun tree-to-list (tree)
    (if (null tree) nil
        (append (tree-to-list (car tree))
                (list (cadr tree))
                (tree-to-list (caddr tree)))))

(defun sort-by-tree (lst)
    (tree-to-list (list-to-tree lst)))

(defun del-neg-num (lst)
    (cond
        ((null lst) nil)
        ((< (car lst) 0) (del-neg-num (cdr lst)))
        (t (cons (car lst) (del-neg-num (cdr lst))))))

;;(quick-sort (append l1 l2)))
(defun merge-list (l1 l2)
    (cond
        ((null l1) l2)
        ((null l2) l1)
        ((< (car l1) (car l2)) (cons (car l1) (merge-list (cdr l1) l2)))
        (t (cons (car l2) (merge-list l1 (cdr l2))))))

(defun dividers (num)
    (loop for i from 2 to (dec num)
          if (eq 0 (mod num i))
              collect i))

(defun q2 (l) (loop for i in l
                    collect (* i i)))

(defun aq (fn l) (loop for i in l
                      collect (funcall fn i)))

(defun repeat (x n)
    (loop repeat n
          collect x))

(defun ^ (x n)
    (reduce #'* (repeat x n)))

((lambda ()
   (labels
           ((t-sum-list      () (sum-list '(1 2 3 4 5)))
            (t-sum-list-r    () (sum-list-r '(1 (2 (3 4)))))
            (t-dec           () (dec 10))
            (t-inc           () (inc 10))
            (t-what          () (what 10))
            (t-tail          () (tail '(1 2 3 4 5 67)))
            (t-range->       () (range-> 10 5))
            (t-range-<       () (range-< 5 10))
            (t-range-to-up   () (range 10 5))
            (t-range-to-dw   () (range 5 10))
            (t-include?      () (include? 7 (range 10 :to 5)))
            (t-remove-el     () (remove-el 8 (range 5 :to 10)))
            (t-set-of        () (set-of '(a b a b a b ab ab ab)))
            (t-unio          () (unio '(a b c d) '(d e a j)))
            (t-intsec        () (intsec '(a b c d) '(d e a j)))
            (t-diff          () (diff '(a b c d) '(d e a j)))
            (t-sim-diff      () (sim-diff '(a s d f g) '(d f g h j k)))
            (t-eq-set        () (eq-set '(1 3 2) '(3 2 1)))
            (t-eq-set2       () (eq-set2 '(1 3 2) '(3 2 1)))
            (t-mkpair        () (mkpair 5 '(1 2 3 4 5)))
            (t-decart        () (decart '(1 2 3 4) '(X Y)))
            (t-add-elt       () (add-elt 4 '((1 2 3))))
            (t-all-subsets   () (all-subsets '(1 2 3)))
            (t-zip           () (zip '(x y z) '(a b c)))
            (t-try-let       () (try-let))
            (t-try-dolist    () (try-dolist '(1 2 3 4 5)))
            (t-try-dotimes   () (try-dotimes 5))
            (t-try-loop      () (try-loop 8))
            (t-try-if        () (try-if 11))
            (t-try-when      () (try-when t))
            (t-try-unless    () (try-unless nil))
            (t-push          () (let ((res '(1 2 7 8))) (push 5 res)))
            (t-iin-lst       () (iin-lst 5 '(1 2 7 8)))
            (t-insert-sort   () (insert-sort '(8 9 572 1 48)))
            (t-split         () (let ((lst '(7 9 100 2 5))) 
                                     (split (car lst) (cdr lst))))
            (t-quick-sort    () (quick-sort '(-89 5 100 85 95 2 48 5)))
            (t-horner        () (horner '(2 -3 5 -8) 2))
            (t-add-to-tree   () (add-to-tree 2 '(nil 10 nil)))
            (t-list-to-tree  () (list-to-tree '(1 5 95 -5 9 0)))
            (t-tree-to-list  () (tree-to-list 
                                 '((NIL -5 (NIL 0 NIL)) 1 (NIL 5 ((NIL 9 NIL) 95 NIL)))))
            (t-sort-by-tree  () (sort-by-tree '(1 5 95 -5 9 0)))
            (t-del-neg-num   () (del-neg-num '(1 2 0 -5 3 -9)))
            (t-merge-list    () (merge-list '(1 12 32) '(2 7 17 89)))
            (t-dividers      () (mapcar #'dividers '(47 100 5)))
            (t-q2            () (q2 '(1 2 3 4 5 6)))
            (t-repeat        () (repeat 2 4))
            (t-^             () (^ 2 4))
            (t-aq            () (aq (lambda (x) (* x x)) '(1 2 3 4 5 6))))
    (format t "~{~a~%~}"
           (list
            (t-q2)
            (t-repeat)
            (t-^)
            (t-aq))))))
