;;;; list
(list 1 2 3 4) ;== '(1 2 3 4)

(print '(lambda () (list "hi")))

;;; plist
(list :a 1 :b 2 :c 3)

;;; getf take plist and smbl
;;; get forward?
(getf (list :a 1 :b 2 :c 3) :a)

(getf (list :a 1 :b 2 :c 3) :c)

;;; It printing next element after the key
(getf (list 1 2 3) 1)

;;; This creates a new entry 
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(make-cd "This is not love" "Kino" 10 t)

;;; Define a new variable by defvar
;;; *earmuffs* - global var
(defvar *db* nil)

;;; This adds a new entry in *db*
(defun add-record (cd) (push cd *db* ))

(add-record (make-cd "Stay with me" "Faces" 8 t))

(add-record (make-cd "Requiem In D Minor" "Amadeus" 10 t))

(add-record (make-cd "Down with the Sickness" "Disturbed" 10 t))

;;; DOLIST take el in *db* and move the value in cd
;;; like a foreach
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(format t "Pre-load dump~%")
(dump-db)

;; Or like this
;; ->
(defun dump-db ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
    (y-or-n-p "Ripped")))

;;; LOOP makes repeat then does not return

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another?")) (return))))

(defun save-db (filename) 
  (with-open-file (out filename
                    :direction :output
                    :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(save-db "./my-db.db")

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun clear-db ()
  (setf *db* nil))

(load-db "./my-db.db")

(format t "After-load dump~%")
(dump-db)

(remove-if-not #'evenp '(1 2 3 4 5 6 7 8))

(remove-if-not (lambda (x) (= 0 (mod x 2 ))) '(1 2 3 4 5 6 7 8))

(defun select-by-artist (artist)
  (remove-if-not 
    #'(lambda (cd) (equal (getf cd :artist) artist))
    *db*))

(defun select (select-fn)
  (remove-if-not select-fn *db*))

(defun artist-select(artist)
  #'(select (lambda (cd) (equal (getf cd :artist) artist))))

(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and 
        (if title (equal (getf cd :title) title) t)
        (if artist (equal (getf cd :artist) artist) t)
        (if rating (equal (getf cd :rating) rating) t)
        (if ripped-p (equal (getf cd :ripped) ripped) t))))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title    (setf (getf row :title) title))
               (if artist   (setf (getf row :artist) artist))
               (if rating   (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

(defmacro backwards (expr) (reverse expr))

(defun make-comparison-expr (field value)
  (list 'equal (list 'getf 'cd field) value))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

(format nil "~r" 1606938044258990275541962092)
