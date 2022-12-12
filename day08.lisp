(in-package :day08)

(defun read-input (&optional (input-path "./example/day08"))
  (with-open-file (s input-path)
    (loop for line = (read-line s nil nil)
          while line
          collect (mapcar (lambda (c)
                            (- (char-code c) (char-code #\0)))
                          (coerce line 'list)))))

(defun build-grid (ll)
  (let* ((m (length ll))
         (n (length (first ll)))
         (grid (make-array (list m n) :element-type 'list :initial-element nil)))
    (loop for row in ll
          for i from 0 do
            (loop for e in row
                  for j from 0 do
                    (setf (aref grid i j) (list e 0))))
    grid))

(defun check-visibilty (grid)
  (let ((cnt 0))
    (destructuring-bind (m n) (array-dimensions grid)
      (macrolet ((h (i j) `(first (aref grid ,i ,j)))
                 (v (i j) `(second (aref grid ,i ,j))))
        (loop for i below m do
          (loop for j below n
                if (or (= i 0) (= i (1- m)) (= j 0) (= j (1- n))) do
                  (setf (v i j) 1)
                  (incf cnt)))
        (loop for i from 1 below (1- m) do
          (loop for j from 1 below (1- n)
                for curr-h = (h i j)
                if (or (loop for ii from 0      below  i      always (< (h ii j) curr-h)) ;; north
                       (loop for ii from (1- m) downto (1+ i) always (< (h ii j) curr-h)) ;; south
                       (loop for jj from (1- n) downto (1+ j) always (< (h i jj) curr-h)) ;; east
                       (loop for jj from 0      below  j      always (< (h i jj) curr-h)));; west
                  do
                     (setf (v i j) 1)
                     (incf cnt)))
        cnt))))

;; 1708
(defun sol1 (&optional (input-path "./input/day08"))
  (-<> (read-input input-path)
    (build-grid <>)
    (check-visibilty <>)))

(defun scenic-score (grid)
  (destructuring-bind (m n) (array-dimensions grid)
    (macrolet ((h  (i j) `(first  (aref grid ,i ,j)))
               (ss (i j) `(second (aref grid ,i ,j))))
      (let ((max 0))
        (loop for i from 0 below m do
          (loop for j from 0 below n do
            (let ((u (loop for ii from (1- i) downto 0 count 1
                           if (>= (h ii j) (h i j)) do
                             (loop-finish)))
                  (l (loop for jj from (1- j) downto 0 count 1
                           if (>= (h i jj) (h i j)) do
                             (loop-finish)))
                  (r (loop for jj from (1+ j) below n count 1
                           if (>= (h i jj) (h i j)) do
                             (loop-finish)))
                  (d (loop for ii from (1+ i) below m count 1
                           if (>= (h ii j) (h i j)) do
                             (loop-finish))))
              (setf (ss i j) (* u l r d)
                    max (if (> max (ss i j)) max (ss i j))))))
        (values max)))))

;; 504000
(defun sol2 (&optional (input-path "./input/day08"))
  (-<> (read-input input-path)
    (build-grid <>)
    (scenic-score <>)))
