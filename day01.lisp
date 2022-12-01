(in-package :day01)

(defun read-input (input-string)
  (with-open-file (s input-string)
    (let ((res nil))
      (loop with tmp
            for line = (read-line s nil nil)
            while line
            if (string= line "") do
              (push tmp res)
              (setf tmp nil)
            else do
              (push (parse-integer line) tmp))
      res)))

(defun sum-up (ll)
  (mapcar (lambda (l)
            (apply #'+ l))
          ll))

(defun lmax (l)
  (loop for v in l maximize v))

;; 70613
(defun sol1 (&optional (input-string "./input/day01"))
  (-<> (read-input input-string)
    (sum-up <>)
    (lmax <>)))

(defun top-three (l)
  (let ((srtd (sort l #'>)))
    (+ (first srtd)
       (second srtd)
       (third srtd))))

;; 205805
(defun sol2 (&optional (input-string "./input/day01"))
  (-<> (read-input input-string)
    (sum-up <>)
    (top-three <>)))
