(in-package #:nlp)

(defstruct (edge
;	     (:type vector)
	     (:print-function
	      (lambda (e s d)
		(declare (ignore d))
		(format s "#<EDGE ~d,~d ~a."
			(left-vertex e) (right-vertex e)
			(label e))
		(if (word e) (format s "\"~A\" " (word e)))
		(if (found e) (format s " Found ~A"
				      (reverse (mapcar (lambda (c)
                                                         (if (edge-p c)
                                                             (label c)
                                                             c))
						       (found e)))))
		(if (to-find e) (format s " Finding ~A" (to-find e)))
		(if (probability e)
		    (format s " P=~F" (probability e)))
		(format s ">")))
	     (:conc-name nil))
  left-vertex
  right-vertex
  label
  to-find
  found
  children
  (word nil)
  probability)

(defun edge-equalp (e1 e2)
  "Edge equality predicate"
  (and (= (left-vertex e1) (left-vertex e2))
       (= (right-vertex e1) (right-vertex e2))
       (eq (label e1) (label e2))
       (equalp (to-find e1) (to-find e2))
       (every (lambda (e1 e2)
                (eq (label e1) (label e2)))
	      (found e1) (found e2))))

(defun sxhash-edge (edge)
  "Hashing algorithm for edges"
  (cond ((edge-p edge)
         (sxhash (list (left-vertex edge) (right-vertex edge) (label edge)
		       (to-find edge) (found edge))))
        (t
         (error "Cannot sxhash-ip a non-edge: ~A" edge))))

(sb-ext:define-hash-table-test edge-equalp sxhash-edge)

(defun make-edge-table ()
  (make-hash-table :test 'edge-equalp))

(defstruct chart (edges #()))

;;; Earley Parser functions
(defun predictor (chart edge)
  "Expand edge"
  (let* ((pos (first (to-find edge)))
	 (p-list (gethash pos (pos-cfg *pos-db*))))
    (when (listp p-list)
      (dolist (p p-list)
	(add-edge chart
		  (make-edge :left-vertex (right-vertex edge)
			     :right-vertex (right-vertex edge)
			     :label pos
			     :to-find (if (listp p) p (list p))
			     :found nil
			     :children nil))))))

(defun completer (chart child-edge)
  "Complete edge"
  (dolist (edge (elt (chart-edges chart) (left-vertex child-edge)))
    (when (eq (label child-edge) (first (to-find edge)))
      (let ((new-edge (make-edge
		       :left-vertex (left-vertex edge)
		       :right-vertex (right-vertex child-edge)
		       :label (label edge)
		       :to-find (rest (to-find edge))
		       :found (cons child-edge (found edge)))))
	(add-edge chart new-edge)))))

(defun complete? (edge)
  "Is edge still searching?"
  (null (to-find edge)))

(defun add-edge (chart edge)
  "Add an edge to the chart"
  (unless (member edge (elt (chart-edges chart) (right-vertex edge))
		  :test 'edge-equalp)
    (push edge (elt (chart-edges chart) (right-vertex edge)))
    (if (complete? edge)
	(completer chart edge)
	(predictor chart edge))))

(defun edge-length (edge)
  "What is the edge's span?"
  (- (right-vertex edge) (left-vertex edge)))

(defun edge->tree (edge)
  "Convert an edge into a parse tree by including its FOUND parts."
  (if (edge-p edge)
      (cons (label edge) (mapcar 'edge->tree (reverse (found edge))))
      edge))

(defun find-trees-in-chart (chart words)
  "Extract the trees from CHART"
  (declare (ignore words))
  (mapcar 'edge->tree
	  (remove-if-not (lambda (e)
                           (and (complete? e)
                                (= (left-vertex e) 0)
                                (eq :start (label e))))
			 (elt (chart-edges chart)
			      (1- (length (chart-edges chart)))))))

(defun earley-parse (text)
  "Earley Parser"
  (multiple-value-bind (initial-pos words) (tag-sentence text)
  ;;(multiple-value-bind (initial-pos words) (possible-tags text)
    (let ((chart (make-chart :edges
			     (make-array (1+ (length words))
					 :initial-element nil))))
      (add-edge chart
		(make-edge :left-vertex 0
			   :right-vertex 0
			   :label :start
			   :to-find '(S)
			   :found nil))
      (flet ((initialize (pos word i)
	       (let ((edge (make-edge :left-vertex i
				      :right-vertex (1+ i)
				      :label pos
				      :word word
				      :to-find nil
				      :found (list word))))
		 (add-edge chart edge))))
	(dotimes (i (length words))
	  (let ((word (nth i words)) (pos (nth i initial-pos)))
	    (cond ((null pos)
		   (format t "Getting possible POS for ~A~%" word)
		   (let ((pos (first (possible-tags word))))
		     (setf (nth i initial-pos)
			   (mapcar (lambda (pair)
                                     (initialize (car pair) word i)
                                     (car pair))
				   pos))))
		  ((listp pos)
		   (dolist (p (reverse pos))
		     (initialize p word i)))
		  (t
		   (initialize pos word i))))))
      (values (find-trees-in-chart chart words) initial-pos))))

;;; Bottom up chart parser functions
(defun add-edge-to-chart (chart edge)
  "Add an edge to the chart"
  (unless (member edge (elt (chart-edges chart) (right-vertex edge))
		  :test 'edge-equalp)
    (push edge (elt (chart-edges chart) (right-vertex edge)))))

(defun bottom-up-rule (chart edge)
  "Chart parser bottom up rule"
  (let ((pos (label edge)))
    (dolist (s (lookup-pos-productions pos))
      (let ((p-list (gethash s (pos-cfg *pos-db*))))
	(dolist (p p-list)
	  (if (listp p)
	      (when (eq (first p) pos)
		(add-edge-to-chart
		 chart (make-edge :left-vertex (left-vertex edge)
				  :right-vertex (left-vertex edge)
				  :label s
				  :to-find p
				  :found nil
				  :children nil)))
	      (when (eq p pos)
		(add-edge-to-chart
		 chart (make-edge :left-vertex (left-vertex edge)
				  :right-vertex (left-vertex edge)
				  :label s
				  :to-find (list p)
				  :found nil
				  :children nil)))))))))

(defun fundamental-rule (chart child-edge)
  "Chart parser fundamental rule"
  (dolist (edge (elt (chart-edges chart) (left-vertex child-edge)))
    (when (and (eq (label child-edge) (first (to-find edge)))
	       (>= (left-vertex child-edge)
		   (left-vertex edge))
	       (/= (right-vertex child-edge)
		   (right-vertex edge)))
      (let ((new-edge (make-edge
		       :left-vertex (left-vertex edge)
		       :right-vertex (right-vertex child-edge)
		       :label (label edge)
		       :to-find (rest (to-find edge))
		       :found (cons child-edge (found edge)))))
	(when (add-edge-to-chart chart new-edge)
	  (when (null (to-find new-edge))
	    (bottom-up-rule chart new-edge)
	    (fundamental-rule chart new-edge)))))))

(defun initialize-word (chart pos word i)
  "Initialize WORD as an edge in chart"
  (let ((edge (make-edge :left-vertex i
			 :right-vertex (1+ i)
			 :label pos
			 :word word
			 :to-find nil
			 :found (list word))))
    (add-edge-to-chart chart edge)
    (bottom-up-rule chart edge)
    (fundamental-rule chart edge)))

(defun chart-parse (text)
  "Bottom up chart parser"
  (multiple-value-bind (initial-pos words) (tag-sentence text)
    (let ((chart (make-chart :edges
			     (make-array (1+ (length words))
					 :initial-element nil))))
      (dotimes (i (length words))
	(let ((word (nth i words)) (pos (nth i initial-pos)))
	  (cond ((null pos)
		 (let ((pos (first (possible-tags word))))
		   (setf (nth i initial-pos)
			 (mapcar (lambda (pair)
                                   (initialize-word chart
                                                    (car pair)
                                                    word i)
                                   (car pair))
				 pos))))
		((listp pos)
		 (dolist (p (reverse pos))
		   (initialize-word chart p word i)))
		(t (initialize-word chart pos word i)))))
      (values (find-trees-in-chart chart words) initial-pos))))

(defun cyk-parse (text)
  "CYK parser. Unfinished."
  (multiple-value-bind (tags words) (tag-sentence text)
    (declare (ignore words))
    (let* ((len (length tags))
	   (non-terminals (make-hash-table))
	   (matrix (make-array `(,len ,len ,(cnf-grammar-size))
			       :initial-element nil)))
      (loop
	 for k being the hash-keys in (pos-cnf-grammar *pos-db*)
	 for i from 0
	 do (setf (gethash k non-terminals) i
		  (gethash i non-terminals) k))
      (dotimes (i len)
	(let ((p (cnf-rev-lookup (nth i tags))))
	  (dolist (s p)
	    (setf (aref matrix i 0 (gethash s non-terminals)) t))))
      (loop for i from 1 to (1- len)
	 do (dotimes (j (- len i))
	      (dotimes (k (1- i))
		(maphash
		 (lambda (s ps)
                   (when (listp ps)
                     (dolist (p ps)
                       (handler-case
                           (when
                               (and
                                (listp p)
                                (aref matrix j k
                                      (gethash (first p) non-terminals))
                                (aref matrix (+ j k) (- i k)
                                      (gethash (second p) non-terminals)))
                             (setf (aref matrix j i (gethash s non-terminals))
                                   t))
                         (error (c)
                           (declare (ignore c))
                           ;;(format t "failure on ~A / ~A~%" s p)
                           )))))
		 (pos-cnf-grammar *pos-db*)))))
      (dotimes (j (1- len))
	(dotimes (k (1- (cnf-grammar-size)))
	  (when (and (aref matrix 0 j k)
		     (eq 'S (gethash k non-terminals)))
	    (return-from cyk-parse t))))
      nil)))
