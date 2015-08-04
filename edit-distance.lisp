(in-package #:nlp)

(defun edit-distance (s1 s2 &key (insert-cost 1) (delete-cost 1) (sub-cost 2))
  "Basic edit distance algorithm with static costs"
  (let* ((s1-length (length s1))
	 (s2-length (length s2))
	 (distance (make-array `(,(1+ s1-length) ,(1+ s2-length))
			       :initial-element 0)))
    (loop for i from 1 to s1-length do
	 (setf (aref distance i 0) (+ (aref distance (1- i) 0) insert-cost)))
    (loop for j from 1 to s2-length do
	 (setf (aref distance 0 j) (+ (aref distance 0 (1- j)) delete-cost)))
    (loop for i from 1 to s1-length do
	 (loop for j from 1 to s2-length do
	      (setf (aref distance i j)
		    (min (+ (aref distance (1- i) j) insert-cost)
			 (+ (aref distance i (1- j)) delete-cost)
			 (+ (aref distance (1- i) (1- j))
			    (if (eql (elt s1 (1- i)) (elt s2 (1- j)))
				0
				sub-cost))))))
    ;;(values (aref distance s1-length s2-length) distance)))
    (aref distance s1-length s2-length)))
