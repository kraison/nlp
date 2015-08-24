(in-package #:nlp)

(defun word-occurance (pos-db word)
  "How many time was word seen in the corpus?"
  (gethash word (pos-word-occurances pos-db)))

(defun add-word-occurance (pos-db word)
  "See the word again"
  (init-or-increment (pos-word-occurances pos-db) word))

(defun add-unigram (pos-db unigram)
  "Add a unigram to the db"
  (incf (pos-total-count pos-db))
  (init-or-increment (pos-unigrams pos-db) unigram))

(defun get-unigram (pos-db unigram)
  "Return a unigram's count"
  (or (gethash unigram (pos-unigrams pos-db)) 0))

(defun add-bigram (pos-db bigram)
  "Add a bigram to the db"
  (init-or-increment (pos-bigrams pos-db) bigram))

(defun get-bigram (pos-db bigram)
  "Return a bigram's count"
  (or (gethash bigram (pos-bigrams pos-db)) 0))

(defun add-trigram (pos-db trigram)
  "Add a trigram to the db"
  (init-or-increment (pos-trigrams pos-db) trigram))

(defun get-trigram (pos-db trigram)
  "Return the trigram's count"
  (or (gethash trigram (pos-trigrams pos-db)) 0))

(defun calculate-gammas (pos-db)
  "Calculate gammas for deleted interpolation"
  (let ((g1 0) (g2 0) (g3 0))
    (maphash
     (lambda (trigram count)
       (let* ((p3 (/-safe (1- count)
                          (1- (get-bigram pos-db (subseq trigram 0 2)))))
              (p2 (/-safe (1- (get-bigram pos-db (subseq trigram 1 3)))
                          (1- (get-unigram pos-db (second trigram)))))
              (p1 (/-safe (1- (get-unigram pos-db (third trigram)))
                          (1- (pos-total-count pos-db)))))
         (cond ((> p3 p2 p1)
                (setq g3 (+ g3 count)))
               ((> p2 p3 p1)
                (setq g2 (+ g2 count)))
               ((> p1 p2 p3)
                (setq g1 (+ g1 count))))))
     (pos-trigrams pos-db))
    (let ((total (+ g1 g2 g3)))
      (values (setf (gethash 1 (pos-gammas pos-db)) (/ g1 total))
	      (setf (gethash 2 (pos-gammas pos-db)) (/ g2 total))
	      (setf (gethash 3 (pos-gammas pos-db)) (/ g3 total))))))

(defun compute-trigram-probability (pos-db trigram)
  "Compute trigram probability"
  (+ (* (gethash 3 (pos-gammas pos-db))
	(/-safe (gethash trigram (pos-trigrams pos-db))
		(get-bigram pos-db (subseq trigram 0 2))))
     (* (gethash 2 (pos-gammas pos-db))
	(/-safe (get-bigram pos-db (subseq trigram 1 3))
		(get-unigram pos-db (second trigram))))
     (* (gethash 1 (pos-gammas pos-db))
	(/-safe (get-unigram pos-db (third trigram))
		(pos-total-count pos-db)))))

(defun compute-ngram-probabilities (pos-db)
  "Compute and store gammas and probabilities for all Ngrams in the db"
  (calculate-gammas pos-db)
  (maphash
   (lambda (trigram count)
     (declare (ignore count))
     (setf (gethash trigram (pos-probabilities pos-db))
           (compute-trigram-probability pos-db trigram)))
   (pos-trigrams pos-db)))

(defun trigram-probability (pos-db trigram)
  "Lookup trigram probability"
  (let ((p (gethash trigram (pos-probabilities pos-db))))
    (if (and (numberp p) (> p 0))
	p
	(compute-trigram-probability pos-db trigram))))

(defun add-observation (pos-db word pos)
  "Add an observation for WORD"
  (unless (hash-table-p (gethash word (pos-observations pos-db)))
    (setf (gethash word (pos-observations pos-db))
	  (make-hash-table :test 'equal)))
  (if (numberp (gethash pos (gethash word (pos-observations pos-db))))
      (incf (gethash pos (gethash word (pos-observations pos-db))))
      (setf (gethash pos (gethash word (pos-observations pos-db))) 1)))

(defun get-observation (pos-db word pos)
  "return observations of WORD as POS"
  (let ((table (gethash word (pos-observations pos-db))))
    (if (hash-table-p table)
	(gethash pos table 0)
	(pos-unknown-probability pos-db))))

(defun get-observations (pos-db word)
  "Get all observations for WORD"
  (let ((table (gethash word (pos-observations pos-db))))
    (if (hash-table-p table)
	(let ((o nil))
	  (maphash (lambda (pos p)
                     (push (cons pos p) o))
		   table)
	  (sort o '> :key 'cdr))
	nil)))

(defun compute-observation-likelihoods (pos-db)
  "Compute all word / POS observation likelihoods"
  (setf (pos-unknown-probability pos-db)
	(/ 1 (hash-table-count (pos-unigrams pos-db))))
  (maphash (lambda (word table)
             (declare (ignore word))
             (maphash (lambda (pos count)
                        (setf (gethash pos table)
                              (/ count (get-unigram pos-db pos))))
                      table))
	   (pos-observations pos-db)))

(defun increment-sentence-markers (pos-db)
  "Add sentence markers to db for a new sentence"
  (add-unigram pos-db *sentence-start*)
  (add-unigram pos-db *sentence-end*)
  (add-word-occurance pos-db "<s>")
  (add-word-occurance pos-db "</s>")
  (add-bigram pos-db (list *sentence-start* *sentence-start*))
  (add-bigram pos-db (list *sentence-end* *sentence-end*))
  (add-observation pos-db "<s>" *sentence-start*)
  (add-observation pos-db "</s>" *sentence-end*))

(defun train-tagger (file &optional pos-db)
  "Train the POS tagger against the corpus FILE. Augent unigrams with lexicon entries."
  (let ((pos-db (or pos-db (make-parts-of-speech)))
	(sentence nil) (pos-seq nil))
    (map-lexicon
     (lambda (word pos-list)
       (dolist (pos pos-list)
         (add-unigram pos-db pos)
         (add-word-occurance pos-db word)
         (add-observation pos-db word pos)))
     pos-db)
    (map-tagged-corpus
     (lambda (word pos)
       (add-unigram pos-db pos)
       (add-word-occurance pos-db word)
       (add-observation pos-db word pos)
       (push pos pos-seq)
       (push word sentence)
       (when (equal pos '|.|)
         (unless (null pos-seq)
           (let ((pos-seq (nreverse pos-seq)))
             (increment-sentence-markers pos-db)
             (add-bigram pos-db
                         (list *sentence-start* (first pos-seq)))
             (add-bigram pos-db
                         (list (last1 pos-seq) *sentence-end*))
             (add-trigram pos-db (list *sentence-start*
                                       *sentence-start*
                                       (first pos-seq)))
             (add-trigram pos-db (list (last1 pos-seq)
                                       *sentence-end*
                                       *sentence-end*))
             (add-trigram pos-db
                          (list *sentence-start*
                                (first pos-seq)
                                (second pos-seq)))
             (add-trigram pos-db (list (second-to-last pos-seq)
                                       (last1 pos-seq)
                                       *sentence-end*))
             (loop for i from 1 to (1- (length pos-seq)) do
                  (add-bigram pos-db (list (elt pos-seq (1- i))
                                           (elt pos-seq i)))
                  (when (> i 1)
                    (add-trigram pos-db (list (elt pos-seq (- i 2))
                                              (elt pos-seq (1- i))
                                              (elt pos-seq i)))))))
         (setq sentence nil pos-seq nil)))
     file)
    (compute-observation-likelihoods pos-db)
    (compute-ngram-probabilities pos-db)
    pos-db))

(defun possible-states (pos-db words)
  "Return all possible POS tags for WORDS"
  (let ((states nil))
    (dolist (word words)
      (let ((w-states (lookup-pos word pos-db)))
	(dolist (w-state w-states)
	  (pushnew w-state states :test 'equal))))
    states))

(defun calculate-path (v words states)
  "Reconstruct the path from the viterbi matrix"
  (let ((path nil))
    (dotimes (j (length words))
      (let ((max 0) (state nil))
	(dotimes (i (length states))
	  (when (> (aref v i j) max)
	    (setq max (aref v i j)
		  state (elt states i))))
	(push state path)))
    (nreverse path)))

(defun possible-tags (text &key p?)
  "Get all possible tags for all words in TEXT. If P?, return probabilities too"
  (let ((words (if (stringp text) (tokenize text) text)))
    (if p?
	(values (mapcar 'get-pos-probabilities words) words)
	(values (mapcar 'lookup-pos words) words))))

(defun tag-sentence (words &key (pos-db *pos-db*) debug)
  "POS tag an individual sentence"
  (let* ((words (if (stringp words) (tokenize words) words))
	 (states (possible-states pos-db words))
	 (viterbi (make-array (list (length states) (length words))
			      :initial-element 0)))
    (when debug (format t "Doing word '~A'~%" (elt words 0)))
    (dotimes (i (length states))
      (let ((p (* (trigram-probability pos-db (list *sentence-start*
						    *sentence-start*
						    (elt states i)))
		  (get-observation pos-db
				   (first words)
				   (elt states i)))))
	(when (and debug (> p 0))
	  (format t "   ~A: trigram: (<s> <s> ~A): ~F~%"
		  (elt words 0)
		  (elt states i)
		  (trigram-probability pos-db
				       (list *sentence-start*
					     *sentence-start*
					     (elt states i)))))
	(setf (aref viterbi i 0) p)))
    (loop for j from 1 to (1- (length words)) do
	 (when debug (format t "Doing word '~A'~%" (elt words j)))
	 (dotimes (i (length states))
           ;; Efficiency hack;  improves accuracy when lexicon is integrated with
           ;; unigrams, kills accuracy when lexicon is not in use
           (when (or (null (lookup-pos (elt words j) pos-db))
                     (is-a (elt words j) (elt states i) pos-db))
             (setf (aref viterbi i j)
                   (* (let ((pp nil))
                        (dotimes (i1 (length states))
                          (dotimes (i2 (length states))
                            (unless (= 0 (aref viterbi i2 (1- j)))
                              (let ((p (* (aref viterbi i2 (1- j))
                                          (trigram-probability
                                           pos-db
                                           (list (elt states i1)
                                                 (elt states i2)
                                                 (elt states i))))))
                                (when (and debug (> p 0))
                                  (format t "   ~A: trigram (~A ~A ~A): ~F~%"
                                          (elt words j)
                                          (elt states i1)
                                          (elt states i2)
                                          (elt states i)
                                          p))
                                (push p pp)))))
                        (if pp (apply 'max pp) 0))
                      ;; FIXME: how do things change if we use the probabilities
                      ;; from the lexicon?
                      (get-observation pos-db
                                       (elt words j)
                                       (elt states i)))))))
    (values (calculate-path viterbi words states) words)))

(defun tag (text)
  "Split TEXT into sentences and tag each one"
  (mapcar (lambda (sentence)
            (nconc
             (multiple-value-list (tag-sentence sentence))
             (list sentence)))
	  (split-sentences text)))

(defun tag-as-text (text)
  "Tag text and return in WORD/TAG format"
  (let ((tags (tag text)))
    (format nil "~{~A~^ ~}"
	    (mapcar (lambda (i)
                      (format nil "~{~A~^ ~}"
                              (mapcar (lambda (tag word)
                                        (format nil "~A/~A" word tag))
                                      (first i) (second i))))
		    tags))))

(defun dump-bigrams (&optional (pos-db *pos-db*))
  (let ((r nil))
    (maphash (lambda (ngram p)
               (push (cons ngram p) r))
	     (pos-bigrams pos-db))
    (sort r 'string> :key 'caar)))

(defun dump-trigrams (&optional (pos-db *pos-db*))
  (let ((r nil))
    (maphash (lambda (ngram p)
               (push (cons ngram p) r))
	     (pos-trigrams pos-db))
    (sort r 'string> :key 'caddar)))

(defun dump-probabilities (&optional (pos-db *pos-db*))
  (let ((r nil))
    (maphash (lambda (ngram p)
               (push (cons ngram p) r))
	     (pos-probabilities pos-db))
    (sort r '> :key 'cdr)))

(defun dump-pos-observations (&optional (pos-db *pos-db*))
  (let ((r nil))
    (maphash (lambda (word table)
               (maphash (lambda (pos p)
                          (push `((,word ,pos) ,p) r))
                        table))
	     (pos-observations pos-db))
    (sort r '> :key 'second)))
