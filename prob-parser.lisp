(in-package :nlp)

;;; Keivn's experimental probabilistic parser.
;;; This works, but not well
;;; The PCP parser in pcp.lisp actually works.
(defun p-find-trees-in-chart (chart words)
  (mapcar 'edge->tree
	  (sort
	   (remove-if-not (lambda (e)
                            (and (complete? e)
                                 (= (left-vertex e) 0)
                                 (eq :start (label e))))
			  (elt (chart-edges chart) (1- (length (chart-edges chart)))))
	   '>
	   :key 'probability)))

(defun p-bottom-up-rule (chart edge)
  (let* ((pos (label edge))
	 (productions (lookup-pos-productions pos)))
    (dolist (s productions)
      (let ((p-hash (gethash s (pos-pcfg *pos-db*))))
	(maphash (lambda (production p)
                   (if (listp production)
                       (when (eq (first production) pos)
                         (add-edge-to-chart
                          chart (make-edge
                                 :left-vertex (left-vertex edge)
                                 :right-vertex (left-vertex edge)
                                 :label s
                                 :to-find production
                                 :found nil
                                 :probability p)))
                       (when (eq production pos)
                         (add-edge-to-chart
                          chart (make-edge
                                 :left-vertex (left-vertex edge)
                                 :right-vertex (left-vertex edge)
                                 :label s
                                 :to-find (list production)
                                 :found nil
                                 :probability p)))))
		 p-hash)))
    (setf (elt (chart-edges chart) (left-vertex edge))
	  (sort (elt (chart-edges chart) (left-vertex edge))
		'> :key 'probability))))

(defun p-fundamental-rule (chart child-edge)
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
	(setf (probability new-edge)
	      (* (probability edge)
		 (reduce '* (found edge) :key 'probability)))
	(when (add-edge-to-chart chart new-edge)
	  (setf (elt (chart-edges chart) (right-vertex new-edge))
		(sort (elt (chart-edges chart)
			   (right-vertex new-edge))
		      '> :key 'probability))
	  (when (null (to-find new-edge))
	    (p-bottom-up-rule chart new-edge)
	    (p-fundamental-rule chart new-edge)))))))

(defun p-initialize-word (chart pos word p i)
  (let ((edge (make-edge :left-vertex i
			 :right-vertex (1+ i)
			 :label pos
			 :word word
			 :to-find nil
			 :found (list word)
			 :probability p)))
    (add-edge-to-chart chart edge)
    (p-bottom-up-rule chart edge)
    (p-fundamental-rule chart edge)))

(defun p-chart-parse (text)
  (multiple-value-bind (pos-with-p words) (possible-tags text :p? t)
    (let ((chart (make-chart :edges
			     (make-array (1+ (length words))
					 :initial-element nil))))
      (dotimes (i (length words))
	(let ((word (nth i words)) (pos-list (nth i pos-with-p)))
	  (dolist (pos (sort pos-list '> :key 'cdr))
	    (unless (zerop (cdr pos))
	      (format t "Initializing ~A(~F) -> ~A~%"
		      word (cdr pos) (car pos))
	      (p-initialize-word chart (car pos) word (cdr pos) i)))))
      ;;chart)))
      (values (p-find-trees-in-chart chart words)
	      (mapcar (lambda (w p)
                        (list w p)) words pos-with-p)))))
