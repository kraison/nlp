(in-package :nlp)

(defun flatten-phrase-tree (tree)
  (let ((leaf-phrases nil))
    (labels ((dfs-helper (this-node phrase-type)
               (cond ((and (consp this-node)
                           (atom (first this-node))
                           (atom (second this-node)))
                      (push (list phrase-type (first this-node)) leaf-phrases))
                     ((and (consp this-node)
                           (atom (first this-node))
                           (consp (second this-node)))
                      (dolist (phrase (rest this-node))
                        (dfs-helper phrase (first this-node)))))))
      (dfs-helper tree nil))
    (nreverse leaf-phrases)))

(defun extract-phrase-patterns (corpus-file)
  (let ((patterns nil))
    (map-parsed-corpus
     (lambda (tree)
       (push (find-all-phrases tree) patterns))
     corpus-file)
    patterns))

(defun np-tag-occurance (pos-db tag)
  "How many time was tag seen in the corpus?"
  (gethash tag (pos-np-tag-occurances pos-db)))

(defun add-np-tag-occurance (pos-db tag)
  "See the tag again"
  (init-or-increment (pos-np-tag-occurances pos-db) tag))

(defun add-np-unigram (pos-db unigram)
  "Add a unigram to the db"
  (incf (pos-np-total-count pos-db))
  (init-or-increment (pos-np-unigrams pos-db) unigram))

(defun get-np-unigram (pos-db unigram)
  "Return a unigram's count"
  (gethash unigram (pos-np-unigrams pos-db) 0))

(defun add-np-bigram (pos-db bigram)
  "Add a bigram to the db"
  (init-or-increment (pos-np-bigrams pos-db) bigram))

(defun get-np-bigram (pos-db bigram)
  "Return a bigram's count"
  (gethash bigram (pos-np-bigrams pos-db) 0))

(defun add-np-trigram (pos-db trigram)
 "Add a trigram to the db"
  (init-or-increment (pos-np-trigrams pos-db) trigram))

(defun get-np-trigram (pos-db trigram)
  "Return the trigram's count"
  (gethash trigram (pos-np-trigrams pos-db) 0))

(defun add-np-observation (pos-db pos-tag np-tag)
  "Add an observation for pos-tag"
  (unless (hash-table-p (gethash pos-tag (pos-np-observations pos-db)))
    (setf (gethash pos-tag (pos-np-observations pos-db))
	  (make-hash-table :test 'equal)))
  (if (numberp (gethash np-tag (gethash pos-tag (pos-np-observations pos-db))))
      (incf (gethash np-tag (gethash pos-tag (pos-np-observations pos-db))))
      (setf (gethash np-tag (gethash pos-tag (pos-np-observations pos-db))) 1)))

(defun get-np-observation (pos-db pos-tag np-tag)
  "return observations of pos-tag as np-tag"
  (let ((table (gethash pos-tag (pos-np-observations pos-db))))
    (if (hash-table-p table)
	(gethash np-tag table 0)
	(pos-np-unknown-probability pos-db))))

(defun get-np-observations (pos-db pos-tag)
  "Get all observations for pos-tag"
  (let ((table (gethash pos-tag (pos-np-observations pos-db))))
    (if (hash-table-p table)
	(let ((o nil))
	  (maphash (lambda (pos p)
                     (push (cons pos p) o))
		   table)
	  (sort o '> :key 'cdr))
	nil)))

(defun compute-np-observation-likelihoods (pos-db)
  "Compute all pos-tag / np-tag observation likelihoods"
  (setf (pos-np-unknown-probability pos-db)
	(/ 1 (hash-table-count (pos-np-unigrams pos-db))))
  (maphash (lambda (pos-tag table)
             (declare (ignore pos-tag))
             (maphash (lambda (np-tag count)
                        (setf (gethash np-tag table)
                              (/ count (get-np-unigram pos-db np-tag))))
                      table))
	   (pos-np-observations pos-db)))

(defun calculate-np-gammas (pos-db)
  "Calculate gammas for deleted interpolation"
  (let ((g1 0) (g2 0) (g3 0))
    (maphash
     (lambda (trigram count)
       (let* ((p3 (/-safe (1- count)
                          (1- (get-np-bigram pos-db (subseq trigram 0 2)))))
              (p2 (/-safe (1- (get-np-bigram pos-db (subseq trigram 1 3)))
                          (1- (get-np-unigram pos-db (second trigram)))))
              (p1 (/-safe (1- (get-np-unigram pos-db (third trigram)))
                          (1- (pos-np-total-count pos-db)))))
         (cond ((> p3 p2 p1)
                (setq g3 (+ g3 count)))
               ((> p2 p3 p1)
                (setq g2 (+ g2 count)))
               ((> p1 p2 p3)
                (setq g1 (+ g1 count))))))
     (pos-np-trigrams pos-db))
    (let ((total (+ g1 g2 g3)))
      (values (setf (gethash 1 (pos-np-gammas pos-db)) (/-safe g1 total))
	      (setf (gethash 2 (pos-np-gammas pos-db)) (/-safe g2 total))
	      (setf (gethash 3 (pos-np-gammas pos-db)) (/-safe g3 total))))))

(defun compute-np-trigram-probability (pos-db trigram)
  "Compute trigram probability"
  (+ (* (gethash 3 (pos-np-gammas pos-db))
	(/-safe (gethash trigram (pos-np-trigrams pos-db))
		(get-np-bigram pos-db (subseq trigram 0 2))))
     (* (gethash 2 (pos-np-gammas pos-db))
	(/-safe (get-np-bigram pos-db (subseq trigram 1 3))
		(get-np-unigram pos-db (second trigram))))
     (* (gethash 1 (pos-np-gammas pos-db))
	(/-safe (get-np-unigram pos-db (third trigram))
		(pos-np-total-count pos-db)))))

(defun np-trigram-probability (pos-db trigram)
  "Lookup np-trigram probability"
  (let ((p (gethash trigram (pos-np-probabilities pos-db))))
    (if (and (numberp p) (> p 0))
	p
	(compute-np-trigram-probability pos-db trigram))))

(defun compute-np-ngram-probabilities (pos-db)
  "Compute and store gammas and probabilities for all Ngrams in the db"
  (calculate-np-gammas pos-db)
  (maphash
   (lambda (trigram count)
     (declare (ignore count))
     (setf (gethash trigram (pos-np-probabilities pos-db))
           (compute-np-trigram-probability pos-db trigram)))
   (pos-np-trigrams pos-db)))

(defun translate-phrase-marker (marker)
  (let ((string (symbol-name marker)))
    (cond ((or (eql (search "NP" string) 0)
               (eql (search "NX" string) 0)
               (eql (search "UCP" string) 0)
               ;;(eql (search "WHNP" string) 0)
               (eql (search "NAC" string) 0))
           'NP)
#|
          ((eql (search "VP" string) 0)
           'VP)
          ((or (eql (search "PP" string) 0)
               (eql (search "WHPP" string) 0))
           'PP)
          ((eql (search "CONJ" string) 0)
           'CONJP)
          ((eql (search "INT" string) 0)
           'INTP)
          ((eql (search "FRAG" string) 0)
           'FRAG)
          ((search "ADJ" string)
           'ADJP)
          ((search "ADV" string)
           'ADVP)
          ((equal "PRT" string)
           'PRT)
          ((eql (search "WH" string) 0)
           'WH)
          ((or (equal "S" string)
               (eql (search "SBAR" string) 0)
               (eql (search "S" string) 0))
           'S)
|#
          (t
           'OTHER))))

(defun increment-np-sentence-markers (pos-db)
  "Add sentence markers to db for a new sentence"
  (add-np-unigram pos-db *sentence-start*)
  (add-np-unigram pos-db *sentence-end*)
  (add-np-tag-occurance pos-db "<s>")
  (add-np-tag-occurance pos-db "</s>")
  (add-np-bigram pos-db (list *sentence-start* *sentence-start*))
  (add-np-bigram pos-db (list *sentence-end* *sentence-end*))
  (add-np-observation pos-db "<s>" *sentence-start*)
  (add-np-observation pos-db "</s>" *sentence-end*))

(defun make-begin-end-markers (seq)
  (let ((in-phrase nil) (new-seq nil))
    (dolist (marker seq)
      (cond ((eql in-phrase marker)
             (push (intern (format nil "~A-IN" marker) :nlp) new-seq))
            (t
             (setq in-phrase marker)
             (push (intern (format nil "~A-BEGIN" marker) :nlp) new-seq))))
    (nreverse new-seq)))

(defun learn-phrase-pattern (pattern &optional (pos-db *pos-db*))
  (let ((pos-seq (mapcar 'second pattern))
        (marker-seq (make-begin-end-markers
                     (mapcar (lambda (pair)
                               (translate-phrase-marker (first pair)))
                             pattern))))
    (dotimes (i (length pos-seq))
      (add-np-unigram pos-db (elt marker-seq i))
      (add-np-tag-occurance pos-db (elt pos-seq i))
      (add-np-observation pos-db (elt pos-seq i) (elt marker-seq i)))
    (increment-np-sentence-markers pos-db)
    (add-np-bigram pos-db
                   (list *sentence-start* (first marker-seq)))
    (add-np-bigram pos-db
                   (list (last1 marker-seq) *sentence-end*))
    (add-np-trigram pos-db (list *sentence-start*
                                 *sentence-start*
                                 (first marker-seq)))
    (add-np-trigram pos-db (list (last1 marker-seq)
                                 *sentence-end*
                                 *sentence-end*))
    (add-np-trigram pos-db
                    (list *sentence-start*
                          (first marker-seq)
                          (second marker-seq)))
    (add-np-trigram pos-db (list (second-to-last marker-seq)
                                 (last1 marker-seq)
                                 *sentence-end*))
    (loop for i from 1 to (1- (length marker-seq)) do
         (add-np-bigram pos-db (list (elt marker-seq (1- i))
                                     (elt marker-seq i)))
         (when (> i 1)
           (add-np-trigram pos-db (list (elt marker-seq (- i 2))
                                        (elt marker-seq (1- i))
                                        (elt marker-seq i)))))))

(defun train-phrase-extractor (file &optional (pos-db *pos-db*))
  "Train the noun phrase extractor on a given labeled corpus."
  (setf (pos-np-total-count pos-db) 0
        (pos-np-gammas pos-db) (make-hash-table)
        (pos-np-probabilities pos-db) (make-hash-table :test 'equal)
        (pos-np-unigrams pos-db) (make-hash-table :test 'equal)
        (pos-np-bigrams pos-db) (make-hash-table :test 'equal)
        (pos-np-trigrams pos-db) (make-hash-table :test 'equal)
        (pos-np-tag-occurances pos-db) (make-hash-table :test 'equal)
        (pos-np-pos-occurances pos-db) (make-hash-table :test 'equal)
        (pos-np-observations pos-db) (make-hash-table :test 'equal)
        (pos-np-unknown-probability pos-db) 0)
  (map-parsed-corpus
   (lambda (tree)
     (learn-phrase-pattern (flatten-phrase-tree tree) pos-db))
   file)
  (compute-np-observation-likelihoods pos-db)
  (compute-np-ngram-probabilities pos-db)
  pos-db)

(defun possible-np-states (pos-db pos-tags)
  "Return all possible phrase tags for WORDS"
  (let ((states nil))
    (dolist (pos-tag pos-tags)
      (let ((w-states (mapcar 'car (get-np-observations pos-db pos-tag))))
	(dolist (w-state w-states)
	  (pushnew w-state states :test 'equal))))
    states))

(defun reconstruct-phrases (markers words pos-tags)
  "Convert NP markers to lists of noun phrase words."
  (let ((phrases nil) (this-phrase nil) (in-phrase nil) (tags nil) (these-tags nil))
    (dotimes (i (length markers))
      (cond ((and (or (eql (elt markers i) 'NP-BEGIN)
                      (eql (elt markers i) 'NP-IN))
                  (not (eql (elt pos-tags i) 'PRP))
                  (not (eql (elt pos-tags i) 'PRP$))
                  (not (eql (elt pos-tags i) 'SYM))
                  (not (eql (elt pos-tags i) '|,|))
                  (not (eql (elt pos-tags i) '|(|))
                  (not (eql (elt pos-tags i) '|)|))
                  (not (eql (elt pos-tags i) '|``|))
                  (not (eql (elt pos-tags i) '|''|))
                  (not (eql (elt pos-tags i) '|:|))
                  (not (eql (elt pos-tags i) 'DT)))
             (if (and in-phrase (not (eql (elt pos-tags i) 'CC)))
                 (progn
                   (push (elt words i) this-phrase)
                   (push (elt pos-tags i) these-tags))
                 (progn
                   (when this-phrase
                     (push (nreverse this-phrase) phrases)
                     (push (nreverse these-tags) tags)
                     (setq this-phrase nil)
                     (setq these-tags nil))
                   (when (not (eql (elt pos-tags i) 'CC))
                     (push (elt words i) this-phrase)
                     (push (elt pos-tags i) these-tags))
                   (setq in-phrase t))))
            (t
             (when (and in-phrase this-phrase)
               (unless (and (= 1 (length this-phrase))
                            (or (eql (elt these-tags 0) 'POS)
                                (eql (elt these-tags 0) 'JJ)))
                 (push (nreverse this-phrase) phrases)
                 (push (nreverse these-tags) tags))
               (setq in-phrase nil
                     these-tags nil
                     this-phrase nil)))))
    (values (nreverse phrases)
            (nreverse tags))))

(defun extract-phrases (sentence &key (pos-db *pos-db*) debug)
  "Extract noun phrases from an individual sentence using Viterbi / HMM strategy."
  (multiple-value-bind (pos-tags words) (tag-sentence sentence)
    (let* ((states (possible-np-states pos-db pos-tags))
           (viterbi (make-array (list (length states) (length pos-tags))
                                :initial-element 0)))
      (when debug (format t "EXTRACT-PHRASES: Doing word '~A/~A'~%" (elt words 0) (elt pos-tags 0)))
      (dotimes (i (length states))
        (let ((p (* (np-trigram-probability pos-db (list *sentence-start*
                                                         *sentence-start*
                                                         (elt states i)))
                    (get-np-observation pos-db
                                        (first pos-tags)
                                        (elt states i)))))
          (when (and debug (> p 0))
            (format t "   ~A: trigram: (<s> <s> ~A): ~F~%"
                    (elt pos-tags 0)
                    (elt states i)
                    (np-trigram-probability pos-db
                                            (list *sentence-start*
                                                  *sentence-start*
                                                  (elt states i)))))
          (setf (aref viterbi i 0) p)))
      (loop for j from 1 to (1- (length pos-tags)) do
           (when debug (format t "Doing tag '~A'~%" (elt pos-tags j)))
           (dotimes (i (length states))
             (setf (aref viterbi i j)
                   (* (let ((pp nil))
                        (dotimes (i1 (length states))
                          (dotimes (i2 (length states))
                            (unless (= 0 (aref viterbi i2 (1- j)))
                              (let ((p (* (aref viterbi i2 (1- j))
                                          (np-trigram-probability
                                           pos-db
                                           (list (elt states i1)
                                                 (elt states i2)
                                                 (elt states i))))))
                                (when (and debug (> p 0))
                                  (format t "   ~A: trigram (~A ~A ~A): ~F~%"
                                          (elt pos-tags j)
                                          (elt states i1)
                                          (elt states i2)
                                          (elt states i)
                                          p))
                                (push p pp)))))
                        (if pp (apply 'max pp) 0))
                      (get-np-observation pos-db
                                          (elt pos-tags j)
                                          (elt states i))))))
      (let ((markers (calculate-path viterbi pos-tags states)))
        (multiple-value-bind (phrases tags)
            (reconstruct-phrases markers words pos-tags)
          (values phrases
                  tags
                  markers
                  pos-tags
                  words))))))

(defun all-phrases (text)
  "Split TEXT into sentences and extract phrases from each one"
  (mapcar (lambda (sentence)
            (nconc
             (multiple-value-list (extract-phrases sentence))
             (list sentence)))
	  (split-sentences text)))
