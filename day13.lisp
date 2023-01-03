(in-package :day13)

(defun read-input (&optional (input-path "./example/day13"))
  (with-open-file (s input-path)
    (loop with pairs
          with curr
          for line = (read-line s nil nil)
          while line
          if (string= "" line) do
            (push (nreverse curr) pairs)
            (setf curr nil)
          else do
            (push line curr)
          finally
             (push (nreverse curr) pairs)
             (return (nreverse pairs)))))

(defun process (lls)
  (loop for (l1 l2) in lls
        for n-l1 = (-<> l1
                     (substitute #\Space #\, <>)
                     (substitute #\( #\[ <>)
                     (substitute #\) #\] <>))
        for n-l2 = (-<> l2
                     (substitute #\Space #\, <>)
                     (substitute #\( #\[ <>)
                     (substitute #\) #\] <>))
        collect (list (with-input-from-string (stream n-l1)
                        (read stream nil nil))
                      (with-input-from-string (stream n-l2)
                        (read stream nil nil)))))

;; = tests ======================
(test regular-lists-of-same-length
  (is (eq t   (apply #'p< '((1 1 3 1 1) (1 1 5 1 1)))))
  (is (eq nil (apply #'p< '((1 1 3 1 1) (1 1 3 1 1)))))
  (is (eq t   (apply #'p< '(((4 4) 4 4) ((4 4) 4 4 4)))))
  (is (eq nil (apply #'p< '((7 7 7 7) (7 7 7)))))
  (is (eq t   (apply #'p< '((7 7 7) (7 7 7 7))))))

(test list-length-tie-break
  (is (eq t (apply #'p< '(((1) (2 3 4)) ((1) 4)))))
  (is (eq t (apply #'p< '(NIL (3))))))

(test type-mismatch-ensure-list
  (is (eq nil (apply #'p< '((9) ((8 7 6)))))))

(test nesting
  (is (eq nil (apply #'p< '(((NIL)) (NIL)))))
  (is (eq nil (apply #'p< '((1 (2 (3 (4 (5 6 7)))) 8 9) (1 (2 (3 (4 (5 6 0)))) 8 9)))))
  (is (eq t   (apply #'p< '((1 (2 (3 (4 (5 6)))) 8 9) (1 (2 (3 (4 (5 6 0)))) 8 9)))))
  (is (eq nil (apply #'p< '((1 (2 (3 (4 (5 6 0)))) 8 9) (1 (2 (3 (4 (5 6 0)))) 8 8))))))
;; ==============================

(defun p< (a b)
  (cond ((and (integerp a) (integerp b))
         (cond ((< a b) -1)
               ((= a b)  0)
               ((> a b)  1)))
        ((and (integerp a) (consp b))   (p< (list a) b))
        ((and (consp a) (integerp b))   (p< a (list b)))
        ((and (null a) (null b))        0)
        ((and (null a) (not (null b))) -1)
        ((and (not (null a)) (null b))  1)
        ((and (consp a) (consp b))
         (let ((res (p< (car a) (car b))))
           (cond ((minusp res) -1)
                 ((plusp res)   1)
                 ((zerop res)   (p< (cdr a) (cdr b))))))))

(defun indices-in-order (lls)
  (loop for (a b) in lls
        for i from 1
        if (minusp (p< a b)) collect i))

;; 5393
(defun sol1 (&optional (input-path "./input/day13"))
  (-<> (read-input input-path)
    (process <>)
    (indices-in-order <>)
    (reduce #'+ <>)))

(defun concat (pairs)
  (loop for pair in pairs nconc pair))

;; 26712
(defun sol2 (&optional (input-path "./input/day13"))
  (-<> (read-input input-path)
    (append <> (list (list "[[2]]" "[[6]]")))
    (process <>)
    (concat <>)
    (sort <> (lambda (x y)
               (case (p< x y)
                 ((-1 0) t)
                 (otherwise nil))))
    (let ((two-pos (1+ (position '((2)) <> :test #'equal)))
          (six-pos (1+ (position '((6)) <> :test #'equal))))
      (* two-pos six-pos))))
