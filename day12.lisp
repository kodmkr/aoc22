(in-package :day12)

(defun read-input (&optional (input-path "./example/day12"))
  (with-open-file (s input-path)
    (loop for line = (read-line s nil nil) while line
          collect line)))

(defparameter *start* nil)
(defparameter *end* nil)
(defparameter *grid* nil)
(defparameter *preds* nil)

(defun process (ll)
  (let* ((nrows (length ll))
         (ncols (length (first ll)))
         (grid (make-array `(,nrows ,ncols))))
    (loop for l in ll
          for i from 0 do
            (loop for c across l
                  for j from 0
                  if (char= c #\S) do
                    (setf *start* (list i j)
                          (aref grid i j) #\a)
                  else if (char= c #\E) do
                    (setf *end* (list i j)
                          (aref grid i j) #\z)
                  else do
                    (setf (aref grid i j) c)))
    (setf *grid* grid)
    grid))

(defun neighbours (grid i j)
  (destructuring-bind (m n) (array-dimensions grid)
    (let* ((posns (list (list (1- i) j) (list i (1+ j))
                        (list (1+ i) j) (list i (1- j))))
           (in-bounds-posns (remove-if (lambda (p)
                                         (or (or (< (first p) 0) (< (1- m) (first p)))
                                             (or (< (second p) 0) (< (1- n) (second p)))))
                                       posns)))
      in-bounds-posns)))

(defun hdiff (curr-pos next-pos)
  (let ((curr-pos-height-val (- (char-code (apply #'aref *grid* curr-pos)) (char-code #\a)))
        (next-pos-height-val (- (char-code (apply #'aref *grid* next-pos)) (char-code #\a))))
    (- next-pos-height-val curr-pos-height-val)))

(defun manh (p q)
  (destructuring-bind ((px py) (qx qy)) (list p q)
    (+ (abs (- px qx)) (abs (- py qy)))))

(defun est (p)
  (manh p *end*))

(defun remove-high-posns (curr-posn posns)
  (remove-if (lambda (pos) (< 1 (hdiff curr-posn pos))) posns))

(defun find-path (start grid)
  (let ((dists (make-hash-table :test #'equal :size 3000))
        (preds (make-hash-table :test #'equal :size 3000)))
    (macrolet ((p (pos) `(gethash ,pos preds))
               (d (pos) `(gethash ,pos dists most-positive-fixnum)))
      (loop
        initially
           (setf (d start) 0)
           (push start q)
        for q = (sort q #'lt)
        while q
        for curr = (pop q)
        for nbrs = (remove-high-posns curr (neighbours grid (first curr) (second curr)))
        do
           (loop for nbr in nbrs
                 if (< (+ (d curr) 1) (d nbr)) do
                   (setf (d nbr) (+ (d curr) 1)
                         (p nbr) curr
                         q (append q (list nbr))))))
    (setf *preds* preds)
    preds))

(defun befs (start grid)
  (let ((dists   (make-hash-table :test #'equal :size 3000))
        (seens   (make-hash-table :test #'equal :size 3000))
        (predecs (make-hash-table :test #'equal :size 3000)))
    (macrolet ((p (pos) `(gethash ,pos predecs))
               (s (pos) `(gethash ,pos seens))
               (d (pos) `(gethash ,pos dists most-positive-fixnum)))
      (loop
        initially
           (push start q)
           (setf (d start) 0
                 (s start) t)
        for q = (sort q (lambda (u v) (< (+ (d u) (est u)) (+ (d v) (est v)))))
        while q
        for curr = (pop q)
        for nbrs = (remove-high-posns curr (neighbours grid (first curr) (second curr)))
        if (equal curr *end*) do
          (setf *preds* predecs)
          (return-from befs predecs)
        else do
          (loop for nbr in nbrs
                if (< (+ (d curr) 1) (d nbr)) do
                  (setf (p nbr) curr
                        (d nbr) (+ (d curr) 1))
                  (when (not (s nbr))
                    (push nbr q)
                    (setf (s nbr) t)))))))

(defun recon-path (start preds &optional (path (list *end*)))
  (let ((next (gethash (first path) preds)))
    (if (equal next start)
        (return-from recon-path path)
        (recon-path start preds (cons next path)))))

;; 462
(defun sol1 (&optional (input-path "./input/day12"))
  (-<> (read-input input-path)
    (process <>)
    (find-path *start* <>)
    (recon-path *start* <>)
    (length <>)))

(defun height-map (file grid)
  (with-canvas (:width 1500 :height 700)
    (destructuring-bind (m n) (array-dimensions grid)
      (set-rgb-fill 0.2 0.4 1.0)
      (set-line-width 0.5)
      (loop for i below m do
        (loop for j below n
              for x = (* j 5)
              for y = (- 500 (* i 10))
              for v = (- (char-code (aref *grid* i j)) (char-code #\a))
              do
                 (set-rgba-fill 0.1 (* (/ 1.0 26) v) 0.7 0.7)
                 (rectangle x y 5 10)
                 (fill-path)))
      (save-png file))))

;; After inspection of the map, it seems that only the first column
;; needs to be checked: the `b's in the second column are the only
;; ones in the whole map.
;; To get to the goal, we must necessarily pass `b' (from the left).
(defun check-as-in-col (col grid)
  (destructuring-bind (m n) (array-dimensions grid)
    (declare (ignorable n))
    (let ((paths nil))
      (loop for i from 0 below m
            for pos = (list i col) do
              (let ((preds (befs pos grid)))
                (push (recon-path pos preds) paths)))
      paths)))

(defun min-path (paths)
  (loop for path in paths
        minimize (length path)))

;; takes long
;; 451
(defun sol2 (&optional (input-path "./input/day12"))
  (-<> (read-input input-path)
    (process <>)
    (check-as-in-col 0 <>)
    (min-path <>)))
