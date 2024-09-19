
(defparameter *player-health* nil
  "Количество здоровья нашего игрока.")

(defparameter *player-agility* nil
  "Количество ловкости нашего игрока.")

(defparameter *player-strength* nil
  "Количество силы нашего игрока.")

(defparameter *monsters* nil
  "Массив, содержащий монстров различных типов")

(defparameter *monster-builders* nil
  "Список функций которые создают наших монстров")

(defparameter *monster-num* 12
  "Количество монстров которых нужно победить.
   Изменяя количество можно менять уровень сложности игры.")

(defun orc-battle ()
  "Главная функция игры.
   Инициализирует состояния, определяет победителя."
  (init-monsters) ; инициализируем монстров и игрока
  (init-player)
  (game-loop) ; Запускаем основной цикл игры
  (when (player-dead) ; игрок проиграл
    (princ "You have been killed. Game Over."))
  (when (monsters-dead) ; игрок победил
    (princ "Congratulations! You have vanquished all of your foes.")))

(defun game-loop ()
  "Обрабатывает повторяющиеся циклы атак монстров и игрока."
  ;; проверяем жив ли игрок или монстры
  (unless (or (player-dead) (monsters-dead))
    ;; показываем некоторую информацию о игроке
    (show-player)
    ;; разрешаем игроку атаковать монстров в зависимости от
    ;; ловкости игрока. При начале игры, у игрока будет 3 атаки
    ;; на более поздних этапах это число снизится до 1-ой.
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
	(show-monsters)
	(player-attack)))
    (fresh-line)
    ;; после атаки игрока, атакуют монстры.
    ;; итерируем список монстров и вызываем monster-attack
    ;; у каждого монстра пока он жив.
    (map 'list
	 (lambda (m)
	   (or (monster-dead m) (monster-attack m)))
	 *monsters*)
    ;; вызываем функция снова
    (game-loop)))

(defun init-player ()
  "Устанавливает начальные значения для игрока"
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  "Проверят умер ли игрок"
  (zerop *player-health*))

(defun show-player ()
  "Показывает состояние игрока"
  (fresh-line)
  (princ "You are a valiant knight with a health of ")
  (princ *player-health*)
  (princ ", an agility of ")
  (princ *player-agility*)
  (princ ", and a strength of ")
  (princ *player-strength*))

(defun player-attack ()
  "Управляет атакой игрока"
  (fresh-line)
  ;; игроку предлагается выбор из 3 атак.
  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  ;; считываем тип атаки и обрабатываем каждый тип
  (case (read)
    ;; одиночная атака, самая сильная
    (s (monster-hit (pick-monster)
		    (+ 2 (randval (ash *player-strength* -1)))))
    ;; двойная атака по 2-м монстрам
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
	 (princ "Your double swing has a strength of ")
	 (princ x)
	 (fresh-line)
	 (monster-hit (pick-monster) x)
	 (unless (monsters-dead)
	   (monster-hit (pick-monster) x))))
    ;; круговая атака по монстрам
    (otherwise
     (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
       (unless (monsters-dead)
	 (monster-hit (random-monster) 1))))))

(defun randval (n)
  "Добавляет случайности к атакам"
  (1+ (random (max 1 n))))

(defun random-monster ()
  "Выбирает монстра для хаотичной круговой атаки и
   гарантирует что выбранный монстр ещё не мертв"
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
	(random-monster)
	m)))

(defun pick-monster ()
  "Позволяет игроку выбрать монстра для атаки"
  (fresh-line)
  ;; выводим подсказу для выбора монстра
  (princ "Monster #:")
  (let ((x (read)))
    ;; убеждаемся что игрок выбрал целое, больше 1 и меньше лимита
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
	(progn
	  (princ "That is not a valid monster number.")
	  (pick-monster))
	(let ((m (aref *monsters* (1- x))))
	  ;; проверяем жив ли монстр чтобы атаковать его
	  (if (monster-dead m)
	      (progn
		(princ "That monster is alread dead.")
		(pick-monster))
	      m)))))

(defun init-monsters ()
  "Инициализирует всех мностров в массиве"
  (setf *monsters*
	(map 'vector
	     (lambda (x)
	       (declare (ignore x))
	       (funcall (nth (random (length *monster-builders*))
			     *monster-builders*)))
	     (make-array *monster-num*))))


(defun monster-dead (m)
  "Проверяет <= 0 здоровья у монстра"
  (<= (monster-health m) 0))

(defun monsters-dead ()
  "Проверяет мертвы ли все монтры"
  (every #'monster-dead *monsters*))

(defun show-monsters ()
  "Отображает список монстров"
  (fresh-line)
  (princ "Your foes:")
  (let ((x 0))
    (map 'list
	 (lambda (m)
	   (fresh-line)
	   (princ "   ")
	   (princ (incf x))
	   (princ ". ")
	   (if (monster-dead m)
	       (princ "**dead**")
	       (progn
		 (princ "(Health=")
		 (princ (monster-health m))
		 (princ ") ")
		 (monster-show m))))
	 *monsters*)))

(defstruct monster (health (randval 10)))

(defmethod monster-hit (m x)
  "Отнимает здоровье у монстра и выводит информацию об этом"
  (decf (monster-health m) x)
  (if (monster-dead m)
      (progn
	(princ "You killed the ")
	(princ (type-of m))
	(princ "! "))
      (progn
	(princ "You hit the ")
	(princ (type-of m))
	(princ ", knocking off ")
	(princ x)
	(princ " health points! "))))

(defmethod monster-show (m)
  "Показывает монстра"
  (princ "A fierce ")
  (princ (type-of m)))

(defmethod monster-attack (m)
  "ничего не делает")

(defstruct (orc (:include monster)) (club-level (randval 8)))
(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club"))

(defmethod monster-attack ((m orc))
  (let ((x (randval (orc-club-level m))))
    (princ "An orc swings his club at you and knocks off ")
    (princ x)
    (princ " of your health points. ")
    (decf *player-health* x)))

(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads."))

(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (princ "The corpse of the fully decapitated and decapacitated hydra falls to the floor!")
      (progn
	(princ "You lop off ")
	(princ x)
	(princ " of the hydra's heads! "))))

(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (princ "A hydra attacks you with ")
    (princ x)
    (princ " of its heads! It also grows back one more head! ")
    (incf (monster-health m))
    (decf *player-health* x)))

(defstruct (slime-mold (:include monster)) (sliminess (randval 5)))
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  (princ "A slime mold with a sliminess of ")
  (princ (slime-mold-sliminess m)))

(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (slime-mold-sliminess m))))
    (princ "A slime mold wraps around your legs and decreases your agility by ")
    (princ x)
    (princ "! ")
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princ "It also squirts in your face, taking away a health point! ")
      (decf *player-health*))))

(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond
      ((= x *player-health*)
       (princ "A brigand hits you with his slingshot, taking off 2 health points! ")
       (decf *player-health* 2))
      ((= x *player-agility*)
       (princ "A brigand catches your leg with his whip, taking off 2 agility points!")
       (decf *player-agility* 2))
      ((= x *player-strength*)
       (princ "A brigand cuts your arm with his whip, taking off 2 strength points! ")
       (decf *player-strength* 2)))))



