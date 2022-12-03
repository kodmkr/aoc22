(in-package :day03)

(defun read-input (&optional (input-path "./example/day03"))
  (with-open-file (s input-path)
    (loop for line = (read-line s nil nil) while line
          collect line)))

(defun compartments (rucksack)
  (let* ((l (length rucksack))
         (compartment-size (floor l 2)))
    (list (subseq rucksack 0 compartment-size)
          (subseq rucksack compartment-size))))

(defun shared-item (compartments)
  (let ((c1 (coerce (first compartments) 'list))
        (c2 (coerce (second compartments) 'list)))
    (intersection c1 c2 )))

(defun priority (item)
  (+ (- (char-code item) (if (lower-case-p item) 96 64))
     (if (lower-case-p item) 0 26)))

(defun sol1 (&optional (input-path "./input/day03"))
  (-<> (read-input input-path)
    (loop for rucksack in <>
        for compartments = (compartments rucksack)
        for shared-item = (first (shared-item compartments))
        sum (priority shared-item))))

(defun find-badge (rucksacks)
  (loop for (u v w) on rucksacks by #'cdddr
        for items-u = (coerce u 'list)
        for items-v = (coerce v 'list)
        for items-w = (coerce w 'list)
        collect (first (remove-duplicates
                        (intersection (intersection items-u items-v) items-w)))))

(defun sol2 (&optional (input-path "./input/day03"))
  (-<> (read-input input-path)
    (find-badge <>)
    (mapcar #'priority <>)
    (apply #'+ <>)))
