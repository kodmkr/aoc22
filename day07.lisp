(in-package :day07)

(defstruct node
  (name ""      :type string)
  (size 0       :type integer)
  (parent nil)
  (children nil :type list))

(defun leaf-p (node)
  (null (node-children node)))

(defun read-input (&optional (input-path "./example/day07"))
  (with-open-file (s input-path)
    (loop for line = (read-line s nil nil) while line collect line)))

(defun build-tree (instrs)
  (let ((tree (make-node :name "-")))
    (push (make-node :name "/" :parent tree) (node-children tree))
    (loop for instr in instrs
          for expl = (ppcre:split #\Space instr)
          with current = tree
          do
             (alexandria:switch ((first expl) :test #'string=)
               ("$" (alexandria:switch ((second expl) :test #'string=)
                      ("ls" #| do noting |# )
                      ("cd" (if (string= (third expl) "..")
                                (setf current (node-parent current))
                                (let* ((dnam (third expl))
                                       (next (find dnam (node-children current) :test #'string= :key #'node-name)))
                                  (setf current next))))))
               ("dir" (let* ((dnam (second expl))
                             (nn (make-node :name dnam :parent current)))
                        (push nn (node-children current))))
               (t (let* ((dnam (second expl))
                         (nn (make-node :name dnam :parent current :size (parse-integer (first expl)))))
                    (push nn (node-children current))))))
    tree))

(defun walk-tree (tree &optional (action #'identity))
  (if (leaf-p tree)
      (progn
        (funcall action tree)
        (node-size tree))
      (let ((sz (loop for c in (node-children tree) sum (walk-tree c action))))
        (setf (node-size tree) sz)
        (funcall action tree)
        sz)))

;; 1447046
(defun sol1 (&optional (input-path "./input/day07"))
  (let ((s 0)
        (tree (build-tree (read-input input-path))))
    (walk-tree tree (lambda (n)
                      (when (and (not (leaf-p n)) (< (node-size n) 100000))
                        (incf s (node-size n)))))
    s))

;; 578710
(defun sol2 (&optional (input-path "./input/day07"))
  (let ((tree (build-tree (read-input input-path))))
          (walk-tree tree) ;; to initially get sizes into all nodes
          (let* ((t-size (node-size tree))
                 (tgt-size (- 70000000 t-size))
                 (candidates nil))
            (walk-tree tree (lambda (n)
                              (when (and (not (leaf-p n)) (> (+ (node-size n) tgt-size) 30000000))
                                (push (node-size n) candidates))))
            (first (sort candidates #'<)))))
