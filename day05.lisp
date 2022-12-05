(in-package :day05)

(defun read-input (&optional (input-path "./example/day05"))
  (with-open-file (s input-path)
    (let ((config (loop for line = (read-line s nil nil)
                        while (not (string= "" line))
                        collect line))
          (instructions (loop for line = (read-line s nil nil)
                              while line
                              collect line)))
      (list (nreverse config) instructions))))

(defun index-positions (index-str)
  (loop for c across index-str
        for i from 0
        if (alphanumericp c) collect i))

(defun sections-from-indices (indices)
  (loop for i in indices collect (list (1- i) (+ i 2))))

(defun process-input (config-n-instructions)
  (destructuring-bind (config instructions) config-n-instructions
    (let* ((index-positions (index-positions (first config)))
           (ary (make-array (length index-positions) :element-type 'list :initial-element nil))
           (instrs (loop for instr in instructions
                         collect (ppcre:register-groups-bind ((#'parse-integer qty from to))
                                     ("move (\\d+) from (\\d+) to (\\d+)" instr)
                                   ;; decrement `from' and `to' right now
                                   ;; to avoid dealing with adjustments later
                                   (list qty (1- from) (1- to))))))
      (symbol-macrolet ((a (aref ary i))) ;; abbreviation
        (loop for cts in (rest config) do
          (loop for idx in index-positions
                for i from 0
                for c = (subseq cts idx (1+ idx))
                if (null a) do
                  (setf a (list c))
                else if (not (string= "" (string-trim " " c))) do
                  (setf a (append a (list c))))))
      (list ary instrs))))

(defun split-at (list n)
  (values (subseq list 0 n)
          (subseq list n)))

(defun execute (state-n-instructions &key keep-order)
  (destructuring-bind (state instructions)
      state-n-instructions
    (symbol-macrolet ((st (aref state to))
                      (sf (aref state from)))
      (loop for (qty from to) in instructions
            for len = (length sf) do
              (multiple-value-bind (init tail)
                  (split-at sf (- len qty))
                (setf sf init
                      st (append st (if keep-order tail (reverse tail)))))))
    state))

(defun read-tops (state)
  (loop for s across state
        for last-index = (1- (length s))
        collect (nth last-index s)))

;; WSFTMRHPP
(defun sol1 (&optional (input-path "./input/day05"))
  (-<> (read-input input-path)
    (process-input <>)
    (execute <>)
    (read-tops <>)
    (apply #'concatenate 'string <>)))

;; GSLCMFBRP
(defun sol2 (&optional (input-path "./input/day05"))
  (-<> (read-input input-path)
    (process-input <>)
    (execute <> :keep-order t)
    (read-tops <>)
    (apply #'concatenate 'string <>)))
