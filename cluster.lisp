(in-package #:nlp)

(defparameter *data*
  '((2 0 4 3 0 1 0 2)
    (0 2 4 0 2 3 0 0)
    (4 0 1 3 0 1 0 1)
    (0 1 0 2 0 0 1 0)
    (0 0 2 0 0 4 0 0)
    (1 1 0 2 0 1 1 3)
    (2 1 3 4 0 2 0 2)))

(defun sum (list)
  (apply #'+ list))

(defun square (x)
  (expt x 2))

(defun euclidean-distance (t1 t2)
  (sqrt (apply #'+ (mapcar #'(lambda (x y) (expt (- x y) 2)) t1 t2))))

(defun distance-matrix (data)
  (loop for i from 0 to (1- (length (first data)))
     collect
       (loop for j from (1+ i) to (1- (length (first data)))
          collect
            (list (1+ i)
                  (1+ j)
                  (euclidean-distance (mapcar #'(lambda (row) (nth j row)) data)
                                      (mapcar #'(lambda (row) (nth i row)) data))))))

(defun compute-centroid (doc-ids data)
  (let ((n (length doc-ids)) (centroid nil))
    (dotimes (i (length (first data)))
      (push (/ (sum (mapcar #'(lambda (id)
                                (nth i (nth id data)))
                            doc-ids))
               n)
            centroid))
    (reverse centroid)))

(defun dot-product (d1 d2)
  (sum (mapcar #'* d1 d2)))

(defun cosine-sim (s1 s2)
  (/ (sum (mapcar #'* s1 s2))
     (* (sqrt (sum (mapcar #'square s1)))
        (sqrt (sum (mapcar #'square s2))))))

(defun best-match (items)
  (let ((best 0))
    (dotimes (i (length items))
      (when (> (nth i items) (nth best items))
        (setq best i)))
    best))

(defun calc-tf-idf (&optional (data *data*))
  (let* ((N (length data))
         (result (make-list N)))
    (dotimes (i N)
      (setf (nth i result) (make-list (length (first data)))))
    (loop for term from 0 to (1- (length (first data))) do
         (let ((nk (sum (mapcar #'(lambda (row)
                                    (if (> (nth term row) 0) 1 0))
                                data))))
           (dotimes (doc N)
             (let ((TFik (nth term (nth doc data))))
               (setf (nth term (nth doc result))
                     (* TFik (log (/ N nk) 2)))))))
    result))

(defun knn (k new-doc data &rest clusters)
  (let ((tf-idf (calc-tf-idf data))
        (new-doc-tf-idf (elt (calc-tf-idf (append *data* (list new-doc)))
                             (length data)))
        (similarity (make-list (length data))))
    (dotimes (i (length tf-idf))
      (setf (nth i similarity) (cosine-sim new-doc-tf-idf (nth i tf-idf))))
    (let ((rank (sort (copy-list similarity) #'>))
          (doc-ids (make-list k))
          (cluster-matches (make-list (length clusters) :initial-element 0))
          (best 0))
      (dotimes (i k)
        (setf (nth i doc-ids) (position (nth i rank) similarity)))
      (dolist (doc-id doc-ids)
        (dotimes (i (length clusters))
          (when (member doc-id (nth i clusters))
            (incf (nth i cluster-matches)))))
      (dotimes (i (length cluster-matches))
        (when (> (nth i cluster-matches) (nth best cluster-matches))
          (setf best i)))
      (values tf-idf new-doc-tf-idf similarity rank doc-ids cluster-matches best))))

(defun single-pass (data)
  (let ((clusters (list (list 0))) (threshold 10.0))
    (loop for i from 1 to (1- (length data)) do
         (format t "Clusters: ~{{~{DOC~A~^ ~}} ~}~%"
                 (mapcar #'(lambda (cluster)
                             (mapcar #'1+ cluster))
                         clusters))
         (let ((centroids (mapcar
                           #'(lambda (cluster)
                               (format t "Centroid of {~{DOC~A~^ ~}} = "
                                       (mapcar #'1+ cluster))
                               (let ((centroid (compute-centroid cluster data)))
                                 (format t "~A~%" centroid)
                                 centroid))
                           clusters)))
           (let ((best-match nil))
             (dotimes (j (length centroids))
               (let ((sim (dot-product (nth i data) (nth j centroids))))
                 (format t "SIM(DOC~A,C~A) = ~F~%" (1+ i) (1+ j) sim)
                 (when (>= sim threshold)
                   (when (or (and best-match (> sim (second best-match)))
                             (null best-match))
                     (setq best-match (list j sim))))))
             (if best-match
                 (progn
                   (format t "Best match for DOC~A is Cluster #~A~%"
                           (1+ i) (1+ (first best-match)))
                   (push i (nth (first best-match) clusters)))
                 (progn
                   (format t "No best match for DOC~A, making new cluster.~%" (1+ i))
                   (setq clusters (append clusters (list (list i)))))))))
    (format t "~{{~{DOC~A~^ ~}} ~}~%" (mapcar #'(lambda (cluster)
                                                  (mapcar #'1+ cluster))
                                              clusters))
    clusters))

(defun k-means (data clusters)
  (let ((k (length clusters)) (centroids nil) (changes? nil))
    (dotimes (i k)
      (format t "Cluster~A: ~A~%" (1+ i) (mapcar #'1+ (nth i clusters)))
      (push (compute-centroid (nth i clusters) data) centroids))
    (setq centroids (reverse centroids))
    (dotimes (i k)
      (format t "Cluster~A centroid: ~A~%" (1+ i) (nth i centroids)))
    (let ((new-clusters (make-list k)))
      (dotimes (i (length data))
        (let ((sims (mapcar #'(lambda (centroid)
                                (cosine-sim centroid (nth i data)))
                            centroids)))
          (dotimes (j (length sims))
            (format t "DOC~A similarity to C~A is ~A~%"
                    (1+ i) (1+ j) (nth j sims)))
          (let ((cluster (best-match sims)))
            (push i (nth cluster new-clusters))
            (when (not (member i (nth cluster clusters)))
              (setq changes? t)))))
      (if changes?
          (k-means data new-clusters)
          (progn
            (dotimes (i k)
              (format t "Cluster~A: ~A~%" (1+ i) (mapcar #'1+ (nth i clusters))))
            new-clusters)))))

;;; Q4
(defparameter *movies*
  '(nil "TRUE BELIEVER" "THE DA VINCI CODE" "THE WORLD IS FLAT" "MY LIFE SO FAR"
    "THE TAKING" "THE KITE RUNNER" "RUNNY BABBIT" "HARRY POTTER"))
(defparameter *u* '((U1 2 4 nil 3 nil nil 1 1)
                    (U2 nil 5 nil nil 3 2 1 nil)
                    (U3 3 nil 1 2 2 nil nil 5)
                    (U4 nil 3 nil nil 4 1 nil 3)
                    (U5 2 4 3 nil nil 2 1 nil)
                    (U6 5 4 nil 3 1 nil 3 1)
                    (U7 1 4 5 5 3 nil nil 4)
                    (U8 2 2 nil nil 4 5 1 nil)
                    (U9 nil nil 4 2 1 nil nil 5)
                    (U10 3 5 1 nil nil nil 4 4)
                    (U11 2 nil 2 4 nil 1 nil 2)
                    (U12 5 4 nil 2 nil 1 1 3)
                    (U13 nil nil 2 nil 4 nil 4 5)
                    (U14 nil 1 2 3 4 nil 5 5)
                    (U15 nil 3 nil nil 5 3 nil 2)
                    (U16 nil 3 2 1 1 nil 4 nil)
                    (U17 1 5 1 2 nil 4 nil 4)
                    (U18 5 nil 4 nil 2 1 3 5)
                    (U19 nil 3 nil 2 nil 4 1 4)
                    (U20 2 5 1 1 5 4 nil 4)))
(defparameter *nu* '((NU1 4 nil 5 3 2 3 nil 4)
                     (NU2 nil 5 2 5 4 nil 2 nil)))
