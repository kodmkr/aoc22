(in-package :day21)

(defun processed (line)
  (let* ((colon-pos (position #\: line))
         (monkey-name (subseq line 0 colon-pos))
         (opd1-start (+ colon-pos 2)))
    (cond ((digit-char-p (char line opd1-start))
           (list monkey-name (parse-integer (subseq line opd1-start))))
          (t (let* ((op-pos (position-if (lambda (c)
                                           (or (char= c #\+) (char= c #\-)
                                               (char= c #\*) (char= c #\/)))
                                         line :start opd1-start))
                    (opd2-start (+ op-pos 2)))
               (list monkey-name (subseq line opd1-start (1- op-pos))
                     (char line op-pos)
                     (subseq line opd2-start)))))))

(defun read-input (&optional (input-path "./example/day21"))
  (with-open-file (s input-path)
    (loop for line = (read-line s nil nil) while line
          collect (processed line))))

(defstruct (nd (:print-function (lambda (m stream depth)
                                  (declare (ignorable depth))
                                  (format stream "~a (~a)"
                                          (nd-name m)
                                          (nd-value m)))))
  (name ""    :type string)
  (value nil)
  (parent nil)
  (left nil)
  (op #'identity)
  (right nil))

(defun func-from-char (c)
  (case c
    (#\+ #'+)
    (#\- #'-)
    (#\* #'*)
    (#\/ #'/)
    ;; for pt 2
    (#\= #'=)))

(defun build-tree (nodes)
  (labels ((aux (name nodes)
             (alexandria:if-let (l (find name nodes :test #'string= :key #'first))
               (case (length (rest l))
                 (1
                  (make-nd :name name :value (second l)))
                 (otherwise
                  (let ((n (make-nd :name name
                                    :op (third l)
                                    :left (aux (second l) nodes)
                                    :right (aux (fourth l) nodes))))
                    (setf (nd-value n) nil
                          (nd-parent (nd-left n)) n
                          (nd-parent (nd-right n)) n)
                    n))))))
    (aux "root" nodes)))

(defun leafp (node)
  (and (null (nd-left node))
       (null (nd-right node))))

(defun eval-tree (node)
  (cond ((leafp node)
         (nd-value node))
        (t (let ((lval (eval-tree (nd-left node)))
                 (rval (eval-tree (nd-right node))))
             (funcall (func-from-char (nd-op node)) lval rval))))) ;; assumption: all non-leaves have two children and an op

;; 169525884255464
(defun sol1 (&optional (input-path "./input/day21"))
  (-<> (read-input input-path)
    (build-tree <>)
    (eval-tree <>)))

(defun modify-tree-base (tree-base)
  (loop for node in tree-base
        if (string= (first node) "root")
          collect (list (first node) (second node) #\= (fourth node))
        else
          if (string= (first node) "humn")
            collect (list (first node) :xxx)
        else collect node))

(defun normalize (tree)
  (cond ((leafp tree)
         (nd-value tree))
        (t
         (let ((val-left (normalize (nd-left tree)))
               (val-right (normalize (nd-right tree))))
           (when (and (integerp val-left) (integerp val-right))
             (let ((nd-val (funcall (func-from-char (nd-op tree)) val-left val-right)))
               (setf (nd-left tree) nil
                     (nd-right tree) nil
                     (nd-value tree) nd-val)
               nd-val))))))

(defun build-expression (tree)
  (cond ((leafp tree)
         (nd-value tree))
        (t
         (let ((val-left (build-expression (nd-left tree)))
               (val-right (build-expression (nd-right tree)))
               (op (if (functionp (nd-op tree)) #\? (nd-op tree))))
           (list op val-left val-right)))))

(defun inv (op)
  (case op
    (#\* #\/)
    (#\/ #\*)
    (#\+ #\-)
    (#\- #\+)
    (#\= #\=)))

(defun solve (eqn)
  (destructuring-bind (value expr)
      (if (integerp (second eqn))
          (list (second eqn) (third eqn))
          (list (third eqn) (second eqn)))
    (labels ((aux (v e)
               (when (eql e :XXX)
                 (return-from aux v))
               (let ((op (first e)))
                 (if (integerp (third e))
                     (aux (funcall (func-from-char (inv op)) v (third e)) (second e))
                     (if (or (char= op #\+) (char= op #\*))
                            (aux (funcall (func-from-char (inv op)) v (second e)) (third e))
                            (let ((f (func-from-char op)))
                              (aux (funcall f (funcall f v (second e))) (third e))))))))
      (aux value expr))))

;; 3247317268284
(defun sol2 (&optional (input-path "./input/day21"))
  (let ((tr (-<> (read-input input-path)
              (modify-tree-base <>)
              (build-tree <>))))
    (normalize tr)
    (-<> (build-expression tr)
      (solve <>))))
