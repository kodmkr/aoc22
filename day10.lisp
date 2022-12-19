(in-package :day10)

(defclass instr ()
  ((name     :initform ""  :initarg :name     :reader   name)
   (cycles   :initform 0   :initarg :cycles   :accessor cycles)
   (operands :initform nil :initarg :operands :accessor operands)))

(defclass noop (instr)
  ((name :initform "noop")
   (cycles :initform 1)))

(defclass addx (instr)
  ((name :initform "addx")
   (cycles :initform 2)))

(defmethod print-object ((o instr) stream)
  (format stream "#<~s ~s ~s>" (name o) (operands o) (cycles o)))

;; replaces `addx' taking two cycles with `noop' and `addx' that takes
;; one cycle
(defun extract-instrs (line)
  (if (char= (char line 0) #\n)
      (list (make-instance 'noop))
      (let* ((split-point (position-if (lambda (c) (char= c #\Space)) line))
             (num-string (subseq line (1+ split-point)))
             (num (parse-integer num-string)))
        (list (make-instance 'noop) (make-instance 'addx :operands (list num))))))

(defun read-input (&optional (input-path "./example/day10"))
  (with-open-file (s input-path)
    (loop for line = (read-line s nil nil)
          while line
          nconc (extract-instrs line))))

(defclass context ()
  ((cycle :initform 1 :initarg :cycle :accessor cycle)
   (reg :initform 1 :initarg reg :accessor reg)))

(defmethod print-object ((o context) stream)
  (format stream "#<~s ~s ~s>" "ctx" (cycle o) (reg o)))

(defmethod one-step ((instr noop) (ctx context))
  (incf (cycle ctx)))

(defmethod one-step ((instr addx) (ctx context))
  (incf (reg ctx) (first (operands instr)))
  (incf (cycle ctx)))

(defun exec (instrs)
  (let ((ctx (make-instance 'context))
        (sample-times (list 20 60 100 140 180 220))
        (signal-strengths nil))
    (loop for instr in instrs do
      (when (and sample-times (= (cycle ctx) (first sample-times)))
        (pop sample-times)
        (push (* (cycle ctx) (reg ctx)) signal-strengths))
      (one-step instr ctx))
    signal-strengths))

;; 13760
(defun sol1 (&optional (input-path "./input/day10"))
  (-<> (read-input input-path)
    (exec <>)
    (reduce #'+ <>)))

(defun draw-pixels (instrs)
  (let* ((ctx (make-instance 'context))
         (screen (make-array (* 6 40) :element-type 'character :initial-element #\.)))
    (loop for instr in instrs
          for cyc = (1- (cycle ctx))
          for reg = (reg ctx)
          for sprite = (list (1- reg) reg (1+ reg)) do
            (when (member (mod cyc 40) sprite)
              (setf (aref screen cyc) #\#))
            (one-step instr ctx))
    screen))

(defun render (screen)
  (loop for i below 6 do
    (loop for j below 40 do
      (format t "~a" (aref screen (+ (* i 40) j))))
    (terpri)))

;; RFKZCPEF
(defun sol2 (&optional (input-path "./input/day10"))
  (-<> (read-input input-path)
    (draw-pixels <>)
    (render <>)))
