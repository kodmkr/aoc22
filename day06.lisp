(in-package :day06)

(defun read-input (&optional (input-path "./example/day0601"))
  (let ((s (alexandria:read-file-into-string input-path)))
    s))

(defun process (signal &key (view-width 4))
  (let ((len (length signal)))
    (loop for i from 0 below (- len view-width)
          for view = (make-array view-width :displaced-to signal
                                            :displaced-index-offset i)
          for uniq = (remove-duplicates view :test #'char-equal)
          if (= view-width (length uniq)) do
            (return-from process (+ i view-width)))))

;; 1538
(defun sol1 (&optional (input-path "./input/day06"))
  (-<> (read-input input-path)
    (process <>)))

;; 2315
(defun sol2 (&optional (input-path "./input/day06"))
  (-<> (read-input input-path)
    (process <> :view-width 14)))
