
(defparameter *nodes* '((living-room (you are in the living-room.
				      a wizard is snoring loudly on the couch.))
			(garden (you are in a beautiful garden.
				 there is a well in front of you.))
			(attic (you are in the attic.
				there is a giant welding torch in the corner.)))
  "содержит список и описание трех местоположений")

(defun describe-location (location nodes)
  "функция описания локации"
  (second (assoc location nodes)))

(defparameter *edges* '((living-room
			 (garden west door)
			 (attic upstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs ladder)))
  "описывает пути по которым игроки могут перемещаться между местами на карте")

(defun describe-path (edge)
  "даёт текстовое описание куда можно переместиться"
  `(there is a ,(third edge) going ,(second edge) from here.))

(defun describe-paths (location edges)
  "генерирует описание для всех рёбер входящих из местоположения"
  (apply #'append (mapcar #'describe-path (rest (assoc location edges)))))

(defparameter *objects* '(whiskey bucket frog chain)
  "Список объектов")

(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden))
  "Отслеживает локацию объектов")

(defun objects-at (loc objs obj-locs)
  "даёт список объектов на локации"
  (labels ((at-loc-p (obj)
	     (eq (second (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc)
  "Даёт описание объектов на локации"
  (labels ((describe-obj (obj)
	     `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defparameter *location* 'living-room
  "отслеживает позицию игрока среди локаций")

(defun look ()
  "описывает всю локацию"
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  "позволяет перемещаеться между локациями"
  (let ((next (find direction
		    (rest (assoc *location* *edges*))
		    :key #'second)))
    (if next
	(progn
	  (setf *location* (first next))
	  (look))
	'(you cannot go that way))))

(defun pickup (object)
  "позволяет брать предметы на локации"
  (cond
    ((member object (objects-at *location* *objects* *object-locations*))
     (push (list object 'body) *object-locations*)
     `(you are now carrying the ,object))
    (t '(you cannot get that.))))

(defun inventory ()
  "отображает предметы в инвентаре"
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defun game-repl ()
  "выполняет игровой цикл"
  (let ((cmd (game-read)))
    (unless (eq (first cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  "читает из строки пользователя комманду и образует её в объект
   лиспа"
  (let ((cmd (read-from-string
	      (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
	     (list 'quote x)))
      (cons (first cmd) (mapcar #'quote-it (rest cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory weld dunk)
  "список допустимых команд")

(defun game-eval (sexp)
  "если команда допустима то мы её выполняем"
  (if (member (first sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))

(defun tweak-text (list caps lit)
  (when list
    (let ((item (first list))
	  (tail (rest list)))
      (cond
	((eq item #\space) (cons item (tweak-text tail caps lit)))
	((member item '(#\! #\? #\.)) (cons item (tweak-text tail t lit)))
	((eq item #\") (tweak-text tail caps (not lit)))
	((or caps lit) (cons (char-upcase item) (tweak-text tail nil lit)))
	(t (cons (char-downcase item) (tweak-text tail nil nil)))))))

(defun game-print (list)
  (princ (coerce (tweak-text (coerce (string-trim "() "
						  (prin1-to-string list))
				     'list)
			     t nil)
		 'string))
  (fresh-line))


(defun have (object)
  (member object (inventory)))

;; (defparameter *chain-welded* nil)

;; (defun weld (subject object)
;;   (if (and (eq *location* 'attic)
;; 	   (eq subject 'chain)
;; 	   (eq object 'bucket)
;; 	   (have 'chain)
;; 	   (have 'bucket)
;; 	   (not *chain-welded*))
;;       (progn (setf *chain-welded* t)
;; 	     '(the chain is now securely welded to the bucket.))
;;       '(you cannot weld like that.)))

;; (defparameter *bucket-filled* nil)

;; (defun dunk (subject object)
;;   (if (and (eq *location* 'garden)
;; 	   (eq subject 'bucket)
;; 	   (eq object 'well)
;; 	   (have 'bucket)
;; 	   *chain-welded*)
;;       (progn
;; 	(setf *bucket-filled* t)
;; 	'(the bucket is now full of water))
;;       '(you cannot dunk like that.)))

(defmacro game-action (command subj obj place &body body)
  (let ((g1 (gensym)) (g2 (gensym)))
    `(let ((,g1 ',subj) (,g2 ',obj))
       (defun ,command (subject object)
	 (if (and (eq *location* ',place)
		  (eq subject ,g1)
		  (eq object ,g2)
		  (have ,g1))
	     ,@body
	     '(i cant ,command like that.)))
	    (pushnew ',command *allowed-commands*))))


(defparameter *chain-welded* nil)

(game-action weld chain bucket attic
  (if (and (have 'bucket) (not *chain-welded*))
      (progn (setf *chain-welded* t)
	     '(the chain is now securely welded to the bucket.))
      '(you do not have a bucket.)))

(defparameter *bucket-filled* nil)

(game-action dunk bucket well garden
  (if *chain-welded*
      (progn
	(setf *bucket-filled* t)
	'(the bucket is now full of water))
      '(the water level is too low to reach.)))

(game-action splash bucket wizard living-room
  (cond
    ((not *bucket-filled*) '(the bucket has nothing in it.))
    ((have 'frog) '(the wizard awakens and sees that you stole his frog.
		    he is so upset he banishes you to the
		    netherworlds- you lose!! the end.))
    (t '(the wizard awakens from his slumber and greets you warmly.
	 he hands you the magic low-carb donut- you win! the end.))))
