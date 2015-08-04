(in-package #:nlp)

(defun /-safe (x y)
  "Safe division"
  (handler-case
      (/ x y)
    (division-by-zero (c)
      (declare (ignore c))
      0)
    (error (c)
      (declare (ignore c))
      0)))

(defmacro while (pred &body body)
  `(loop (unless ,pred (return nil)) ,@body))

;;; Thanks, Mr. Norvig for this queueing code
(defun print-queue (q stream depth)
  (format stream "<QUEUE: ~a>" (queue-elements q)))

(defstruct (queue
             (:print-function print-queue))
  (key #'identity)
  (last nil)
  (elements nil))

(defun make-empty-queue () (make-queue))

(defun empty-queue? (q)
  (= (length (queue-elements q)) 0))

(defun queue-front (q)
  (elt (queue-elements q) 0))

(defun dequeue (q)
  (when (listp (queue-elements q))
    (pop (queue-elements q))))

(defun enqueue (q items)
  (cond ((null items) nil)
        ((or (null (queue-last q)) (null (queue-elements q)))
         (setf (queue-last q) (last items)
               (queue-elements q) (nconc (queue-elements q) items)))
        (t (setf (cdr (queue-last q)) items
                 (queue-last q) (last items)))))

(defun queue-length (q)
  (length (queue-elements q)))

(defun find-anywhere (item tree)
  "Does item occur anywhere in tree?"
  (format t "Checking for ~A in ~A~%" item tree)
  (if (atom tree)
      (if (eql item tree) tree)
      (or (find-anywhere item (first tree))
	  (find-anywhere item (rest tree)))))

(defun last1 (list)
  "Return the last item in a list"
  (car (last list)))

(defun second-to-last (list)
  "Return the second to last item in a list"
  (let ((l (length list)))
    (when (> l 1)
      (car (subseq list (- l 2) (1- l))))))

(defun flatten (lis)
  "Flatten a tree into a list"
  (cond ((atom lis) lis)
	((listp (car lis))
	 (append (flatten (car lis)) (flatten (cdr lis))))
	(t (append (list (car lis)) (flatten (cdr lis))))))

(defun init-or-increment (hash thing)
  "Increment or add a value for this hash key"
  (incf (gethash thing hash 0)))

(defun join (list)
  "Join a list into a space-delimited string"
  (format nil "~{~A~^ ~}" list))
