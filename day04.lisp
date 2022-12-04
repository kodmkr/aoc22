(in-package :day04)

(defun processed (line)
  (ppcre:register-groups-bind ((#'parse-integer r1s r1e r2s r2e))
      ("(\\d+)-(\\d+),(\\d+)-(\\d+)" line)
    (list (list r1s r1e) (list r2s r2e))))

(defun read-input (&optional (input-path "./example/day04"))
  (with-open-file (s input-path)
    (loop for line = (read-line s nil nil) while line
          collect (processed line))))

(defun contained (r1 r2)
  (destructuring-bind ((r1s r1e) (r2s r2e)) (list r1 r2)
    (and (<= r1s r2s r1e)
         (<= r1s r2e r1e))))

;; 562
(defun sol1 (&optional (input-path "./input/day04"))
  (-<> (read-input input-path)
    (loop for (r1 r2) in <>
          if (or (contained r1 r2)
                 (contained r2 r1))
            count 1)))

(defun overlap-p (r1 r2)
  (destructuring-bind ((r1s r1e) (r2s r2e)) (list r1 r2)
    (declare (ignorable r2e))
    (or (contained r1 r2)
        (<= r1s r2s r1e))))

;; 924
(defun sol2 (&optional (input-path "./input/day04"))
  (-<> (read-input input-path)
    (loop for (r1 r2) in <>
          if (or (overlap-p r1 r2)
                 (overlap-p r2 r1))
            count 1)))
