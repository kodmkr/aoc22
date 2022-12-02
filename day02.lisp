(in-package :day02)

(defun read-input (input-string)
  (with-open-file (s input-string)
    (loop for line = (read-line s nil nil)
          while line
          collect (ppcre:split " " line))))

(defun check-round (opp me)
  (alexandria:switch ((list opp me) :test #'equal)
    ('("A" "X") 3)
    ('("A" "Y") 6)
    ('("A" "Z") 0)
    ('("B" "X") 0)
    ('("B" "Y") 3)
    ('("B" "Z") 6)
    ('("C" "X") 6)
    ('("C" "Y") 0)
    ('("C" "Z") 3)
    ;; added after part 2
    ('("A" "A") 3)
    ('("A" "B") 6)
    ('("A" "C") 0)
    ('("B" "A") 0)
    ('("B" "B") 3)
    ('("B" "C") 6)
    ('("C" "A") 6)
    ('("C" "B") 0)
    ('("C" "C") 3)
    ))

(defun value-of (me)
  (alexandria:switch (me :test #'string=)
    ("X" 1)
    ("Y" 2)
    ("Z" 3)
    ;; added after part 2
    ("A" 1)
    ("B" 2)
    ("C" 3)
    ))

(defun collect-scores (rounds)
  (loop for round in rounds
        collect (+ (check-round (first round) (second round))
                   (value-of (second round)))))

(defun score (scores)
  (apply #'+ scores))

(defun find-move-to-play (opp strategy)
  (alexandria:switch ((list opp strategy) :test #'equal)
    ('("A" "X") "C")
    ('("A" "Y") "A")
    ('("A" "Z") "B")
    ('("B" "X") "A")
    ('("B" "Y") "B")
    ('("B" "Z") "C")
    ('("C" "X") "B")
    ('("C" "Y") "C")
    ('("C" "Z") "A")))

(defun apply-strategy (rounds)
  (loop for (o s) in rounds
        collect (list o (find-move-to-play o s))))

;; 14163
(defun sol1 (&optional (input-path "./input/day02"))
  (-<> (read-input input-path)
    (collect-scores <>)
    (score <>)))

;; 12091
(defun sol2 (&optional (input-path "./input/day02"))
  (-<> (read-input input-path)
    (apply-strategy <>)
    (collect-scores <>)
    (score <>)))
