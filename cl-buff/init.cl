;gnu clisp  2.49.60

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

(defun try-mapcar (lst)
    (mapcar (lambda (x) (* x x)) lst))

(defun try-maplist (lst)
    (maplist (lambda (l) (apply #'* l)) lst))

(defun try-remove-if (lst)
    (remove-if #'evenp lst))
; remove-if[-not] :count n :from-end t

(defun try-remove-if-not (lst)
    (remove-if-not #'evenp lst))

(defun try-every (lst)
    (every (lambda (x) (if (< x 10) t nil)) lst))

(defun try-reduce (lst)
    (reduce #'max lst))
; reduce f l :initial-value n

;closure
(defun dec-closures ()
    (let ((c 0))
         (defun next-1 () (setq c (inc c))))

    (let ((c 0))
         (defun next-2 () (setq c (inc c))))

    (let ((c 0))
         (defun next-3  () (setq c (inc c)))
         (defun reset-3 () (setq c 0)))
    t)

(defun make-adder (x)
    ;(function (lambda (y) (+ x y)))
    ; analog ->
    #'(lambda (y) (+ x y)))

(defun let-lambda-analog ()
    (let ((x 111) (y 111))
         (list x y (+ x y)))
    ; analog ->
    ((lambda (x y)
         (list x y (+ x y))) 111 222))

(defun y-combinator (f lst)
    (funcall f lst f))

(defun sum-by-y-combinator ()
    (y-combinator (lambda (lst f)
                          (if (null lst) 0
                              (+ (car lst) (funcall f (cdr lst) f)))) 
                  (range 6 :to 1)))

(defun lambda-sum-list (lst)
    ((lambda (lst f)
             (if (null lst) 0
                 (+ (car lst) (funcall f (cdr lst) f))))
     lst
     (lambda (lst f)
             (if (null lst) 0
                 (+ (car lst) (funcall f (cdr lst) f))))))

(defun my-mapcar (f lst)
    (reverse
        (reduce (lambda (acc x)
                        (cons (funcall f x) acc)) lst :initial-value nil)))

(defun my-remove-if (p lst)
    (reverse
     (reduce (lambda (acc x)
                     (if (funcall p x) acc
                         (cons x acc)))
             lst :initial-value nil)))

; написать rev (reverse)
; '(6 5 4 3 2 1 0) ->
; '(0 1 2 3 4 5 6)
; если список пустой то мы вернём nil
; иначе если нет, то идём по списку по не встретим конец
(defun rev (lst &optional (acc ()))
    (if (null lst) acc
        (rev (cdr lst) (cons (car lst) acc))))

; выразить mapcar через maplist
(defun mapcar-by-maplist (f lst)
    (maplist (lambda (x) (funcall f (car x))) lst))

; Создать генератор чисел Фибоначчи
;; 1 1 2 3 5 8 13 (сброс, запуск сначала)
(defun fibo-generator ()
    (let ((num1 0) (num2 1))
         (defun fibo-get () num1)
         (defun fibo-next ()
             (let ((new-num (+ num1 num2))) (setq num1 num2) (setq num2 new-num)))
         (defun fibo-reset () (setq num1 0) (setq num2 1) t)))

(defun ncalls (f n)
    (dotimes (i n)
          (funcall f)) t)

; превратить (a b ( c d ( e f))) ->
; ((a 0) (b 0) ( (c 1) (d 1) ( (e 2) (f 2))))
(defun depth-set-r (lst &optional (d 0))
    (cond 
        ((null lst) nil)
        ((atom (car lst)) (cons (list (car lst) d) (depth-set-r (cdr lst) d)))
        (t (cons (depth-set-r (car lst) (inc d)) (depth-set-r (cdr lst) d)))))

(defun depth-set-f (lst &optional (d 0))
    (mapcar (lambda (x) (if (atom x) (list x d) (depth-set-f x (inc d)))) lst))

(defun depth-set-i (lst &optional (d 0))
     (loop for x in lst
           collect (if (atom x) (list x d) (depth-set-i x (inc d)))))

(defparameter *6ell* (range 1 :to 6))
(defparameter *abcl-r* '(a b (c d ((e f) g) h) i))

; rplaca *some* *to-some* like (setq l '(1 2 3)) (rplaca l (cddr l))
;; -> l = '(3 2 3)
; rplacd *some* *to-some* like (setq l '(1 2 3)) (rplacd (cddr l) l)
;; -> l = '(1 2 3 1 2 3 ... inf)

; (gc) to call garbage collector

(defun macro-test ()
    (defun push-m-f (a stack)
        (setq stack (cons a stack)))
    (list
     (setq s '(b a))
     (push-m-f 'c s)
     s)
    ; not work --^ ((B A) (C B A) (B A))
    
    (defmacro push-m-s (a stack)
        (list 'setq stack (list 'cons a stack)))
    (list
     (setq s '(b a))
     (push-m-s 'c s)
     s)
    ; work --^ ((B A) (C B A) (C B A))
    
    ; Обратная блокировка backquote `
    (setq xx '(1 1 1))
    
    `(a s ,xx) ; -> (A S (1 1 1))
    
    `(a s ,@xx) ; -> (A S 1 1 1)
    
    (defmacro push-m-t (a stack) ; analog push-m-s
        `(setq ,stack (cons ,a ,stack)))
    
    ; to see macro res use macroexpand
    (defmacro double (x)
        (cond 
            ((numberp x) `(+ ,x ,x))
            ((atom x) `(setq ,x (+ ,x ,x)))
            (t (print "double: param not atom"))))
    
    ;(macroexpand (double 'a))
    
    (defmacro fq (n)
        (if (= n 1) 1
            (let ((n1 (- n 1)))
                 `(* ,n (fq ,n1)))))
    
    (defmacro copy-list-m (x)
        (if (null x) nil
            (let ((ca (car x)) (cd (cdr x)))
                 `(cons (quote ,ca) (copy-list-m ,cd)))))
    
    (defmacro ntimes (n &rest body)
        `(loop for x from 0
               when (>= x ,n) 
               return 'ok
               do ,@body))
    
    (ntimes 5 (print '*)) ; OK?
    
    (setq *x* 5)
    (ntimes 5 (setq *x* (inc *x*)))
    (print *x*) ; OK?
    
    (let ((x 10))
         (ntimes 5 (setq x (inc x)))
         (print x)) ; WRONG!
    
    ;right version
    (defmacro ntimes-t (n &rest body)
        (let ((x (gensym)))
            `(loop for ,x from 0
                   when (>= ,x ,n) 
                   return 'ok
                   do ,@body)))
    (let ((x 10))
         (ntimes-t 5 (setq x (inc x)))
         (print x)) ; OK!
    
    (let ((v 10))
         (ntimes-t (setq v (dec v)) (print '*))) ; SHIIIT
    (print "~%")
    
    ; ultra right version
    (defmacro ntimes-true (n &rest body)
        (let ((x (gensym)) (h (gensym)))
          `(let ((,h ,n))
             (loop for ,x from 0
                  when (>= ,x ,h) 
                  return 'ok
                  do ,@body ))))
    
    (let ((v 10))
     (ntimes-true (setq v (dec v)) (print '*))) ; OK!
    
    )

((lambda ()
   (labels
           ((t-sum-list            () (sum-list '(1 2 3 4 5)))
            (t-sum-list-r          () (sum-list-r '(1 (2 (3 4)))))
            (t-dec                 () (dec 10))
            (t-inc                 () (inc 10))
            (t-what                () (what 10))
            (t-tail                () (tail '(1 2 3 4 5 67)))
            (t-range->             () (range-> 10 5))
            (t-range-<             () (range-< 5 10))
            (t-range-to-up         () (range 10 5))
            (t-range-to-dw         () (range 5 10))
            (t-include?            () (include? 7 (range 10 :to 5)))
            (t-remove-el           () (remove-el 8 (range 5 :to 10)))
            (t-set-of              () (set-of '(a b a b a b ab ab ab)))
            (t-unio                () (unio '(a b c d) '(d e a j)))
            (t-intsec              () (intsec '(a b c d) '(d e a j)))
            (t-diff                () (diff '(a b c d) '(d e a j)))
            (t-sim-diff            () (sim-diff '(a s d f g) '(d f g h j k)))
            (t-eq-set              () (eq-set '(1 3 2) '(3 2 1)))
            (t-eq-set2             () (eq-set2 '(1 3 2) '(3 2 1)))
            (t-mkpair              () (mkpair 5 '(1 2 3 4 5)))
            (t-decart              () (decart '(1 2 3 4) '(X Y)))
            (t-add-elt             () (add-elt 4 '((1 2 3))))
            (t-all-subsets         () (all-subsets '(1 2 3)))
            (t-zip                 () (zip '(x y z) '(a b c)))
            (t-try-let             () (try-let))
            (t-try-dolist          () (try-dolist '(1 2 3 4 5)))
            (t-try-dotimes         () (try-dotimes 5))
            (t-try-loop            () (try-loop 8))
            (t-try-if              () (try-if 11))
            (t-try-when            () (try-when t))
            (t-try-unless          () (try-unless nil))
            (t-push                () (let ((res '(1 2 7 8))) (push 5 res)))
            (t-iin-lst             () (iin-lst 5 '(1 2 7 8)))
            (t-insert-sort         () (insert-sort '(8 9 572 1 48)))
            (t-split               () (let ((lst '(7 9 100 2 5))) 
                                           (split (car lst) (cdr lst))))
            (t-quick-sort          () (quick-sort '(-89 5 100 85 95 2 48 5)))
            (t-horner              () (horner '(2 -3 5 -8) 2))
            (t-add-to-tree         () (add-to-tree 2 '(nil 10 nil)))
            (t-list-to-tree        () (list-to-tree '(1 5 95 -5 9 0)))
            (t-tree-to-list        () (tree-to-list 
                                       '((NIL -5 (NIL 0 NIL)) 1 (NIL 5 ((NIL 9 NIL) 95 NIL)))))
            (t-sort-by-tree        () (sort-by-tree '(1 5 95 -5 9 0)))
            (t-del-neg-num         () (del-neg-num '(1 2 0 -5 3 -9)))
            (t-merge-list          () (merge-list '(1 12 32) '(2 7 17 89)))
            (t-dividers            () (mapcar #'dividers '(47 100 5)))
            (t-q2                  () (q2 '*6ell*))
            (t-repeat              () (repeat 2 4))
            (t-^                   () (^ 2 4))
            (t-aq                  () (aq (lambda (x) (* x x)) *6ell*))
            (t-try-mapcar          () (try-mapcar *6ell*))
            (t-try-maplist         () (try-maplist *6ell*))
            (t-try-remove-if       () (try-remove-if *6ell*))
            (t-try-remove-if-not   () (try-remove-if-not *6ell*))
            (t-try-every           () (try-every *6ell*))
            (t-try-reduce          () (try-reduce '(1 3 99 4 576 5 4 -6432 0 523 5)))
            (t-dec-closures        () (if (dec-closures) 'dec-closures nil))
            (t-closure             () (list (next-1) (next-1) (next-2)))
            (t-closure2            () (list (next-3) (next-3) (reset-3) (next-3)))
            (t-make-adder          () (setq 3+ (make-adder 5)) (funcall 3+ 6))
            (t-let-lambda-analog   () (let-lambda-analog))
            (t-lambda-sum-list     () (lambda-sum-list (range 6 :to 1)))
            (t-sum-by-y-combinator () (sum-by-y-combinator))
            (t-my-mapcar           () (my-mapcar #'evenp *6ell*))
            (t-my-remove-if        () (my-remove-if #'evenp *6ell*))
            (t-ncalls              () (ncalls (lambda () 5) 5))
            (t-rev                 () (rev (range 6 :to 0)))
            (t-mapcar-by-maplist   () (list (mapcar-by-maplist (lambda (x) (* x x)) *6ell*) (mapcar (lambda (x) (* x x)) *6ell*)))
            (t-fibo-generator      () (fibo-generator) (list (fibo-get) (ncalls #'fibo-next 140) (fibo-get) (fibo-reset) (fibo-get)))
            (t-depth-set-r         () (depth-set-r *abcl-r*))
            (t-depth-set-f         () (depth-set-f *abcl-r*))
            (t-depth-set-i         () (depth-set-i *abcl-r*))
            (t-macro-test          () (macro-test))
            )
    (format t "~{Test result: ~a~%~}"
           (list
            )))))
