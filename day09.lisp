(in-package :day09)

(defun map-dir (dir)
  (alexandria:switch (dir :test #'char=)
    (#\U #c(0 1))
    (#\D #c(0 -1))
    (#\L #c(-1 0))
    (#\R #c(1 0))))

(defun extracted (line)
  (ppcre:register-groups-bind (dir (#'parse-integer amount))
      ("(R|U|L|D) (\\d+)" line)
    (list (map-dir (char dir 0)) amount)))

(defun read-input (&optional (input-path "./example/day09"))
  (with-open-file (s input-path)
    (loop for line = (read-line s nil nil)
          while line
          collect (extracted line))))

(defun touchingp (x y)
  (let ((r (- x y)))
    (or (= r #c(1 1))
        (= r #c(1 0))
        (= r #c(1 -1))
        (= r #c(0 -1))
        (= r #c(-1 -1))
        (= r #c(-1 0))
        (= r #c(-1 1))
        (= r #c(0 1))
        (= r #c(0 0)))))

(defun same-row-or-col-p (x y)
  (or (zerop (- (realpart x) (realpart y)))
      (zerop (- (imagpart x) (imagpart y)))))

(defun diag (z)
  (let ((r (realpart z))
        (i (imagpart z)))
    (complex (* (signum r) (if (zerop r) 0 (floor r r)))
             (* (signum i) (if (zerop i) 0 (floor i i))))))

;; after part 2
(defun execute (instrs &key (rope-length 10))
  (let* ((rope (make-array rope-length :initial-element #c(0 0)))
         (rl (length rope))
         (mi (1- rl))
         (tposs nil))
    (macrolet ((r (i) `(aref rope ,i)))
      (loop for (d amt) in instrs do
        (loop repeat amt do
          (incf (r 0) d)
          (loop for i from 0 below mi do
            (when (not (touchingp (r i) (r (1+ i))))
              (incf (r (1+ i)) (diag (- (r i) (r (1+ i)))))))
          (pushnew (r mi) tposs :test #'=))))
    tposs))

;; 5902
(defun sol1 (&optional (input-path "./input/day09"))
  (-<> (read-input input-path)
    (execute <> :rope-length 2)
    (length <>)))

;; 2445
(defun sol2 (&optional (input-path "./input/day09"))
  (-<> (read-input input-path)
    (execute <> :rope-length 10)
    (length <>)))
