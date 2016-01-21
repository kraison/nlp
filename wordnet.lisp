(in-package :nlp)

(defclass synset ()
  ((synset-id :accessor synset-id :initform nil :initarg :synset-id)
   (synset-type :accessor synset-type :initform nil :initarg :synset-type)
   (sense-number :accessor sense-number :initform nil :initarg :sense-number)
   (tag-count :accessor tag-count :initform nil :initarg :tag-count)
   (synset-gloss :accessor synset-gloss :initform nil :initarg :synset-gloss)
   (hypernym-ptrs :accessor hypernym-ptrs :initform nil :initarg :hypernym-ptrs)
   (hyponym-ptrs :accessor hyponym-ptrs :initform nil :initarg :hyponym-ptrs)
   (instance-ptrs :accessor instance-ptrs :initform nil :initarg :instance-ptrs)
   (entailment-ptrs :accessor entailment-ptrs :initform nil :initarg :entailment-ptrs)
   (similar-ptrs :accessor similar-ptrs :initform nil :initarg :similar-ptrs)
   (member-meronym-ptrs :accessor member-meronym-ptrs :initform nil :initarg :member-meronym-ptrs)
   (substance-meronym-ptrs :accessor substance-meronym-ptrs :initform nil :initarg :substance-meronym-ptrs)
   (part-meronym-ptrs :accessor part-meronym-ptrs :initform nil :initarg :part-meronym-ptrs)
   (member-holonym-ptrs :accessor member-holonym-ptrs :initform nil :initarg :member-holonym-ptrs)
   (substance-holonym-ptrs :accessor substance-holonym-ptrs :initform nil :initarg :substance-holonym-ptrs)
   (part-holonym-ptrs :accessor part-holonym-ptrs :initform nil :initarg :part-holonym-ptrs)
   (morphology-ptrs :accessor morphology-ptrs :initform nil :initarg :morphology-ptrs)
   (class-ptrs :accessor class-ptrs :initform nil :initarg :class-ptrs)
   (cause-ptrs :accessor cause-ptrs :initform nil :initarg :cause-ptrs)
   ;; still need grouped-verbs, attributes, antonyms, see-also, participles, pertainyms, frames

   ;; Aggregations
   (semantic-neighborhood-ptrs :accessor semantic-neighborhood-ptrs :initform nil :initarg :semantic-neighborhood-ptrs)
   (meronym-ptrs :accessor meronym-ptrs :initform nil :initarg :meronym-ptrs)
   (holonym-ptrs :accessor holonym-ptrs :initform nil :initarg :holonym-ptrs)
   (semantic-parent-ptrs :accessor semantic-parent-ptrs :initform nil :initarg :semantic-parent-ptrs)
   (semantic-child-ptrs :accessor semantic-child-ptrs :initform nil :initarg :semantic-child-ptrs)

   (synset-words :accessor synset-words
                 :initform (make-array '(0)
                                       :adjustable t
                                       :element-type 'string)
                 :initarg :synset-words)))

(defgeneric synset-p (synset)
  (:method ((synset synset)) t)
  (:method (thing) nil))

(defmethod print-object ((synset synset) stream)
  (format stream "#<SYNSET ~A: ~S>" (synset-id synset) (synset-words synset)))

(defmethod synset-eq ((synset1 synset) (synset2 synset))
  (= (synset-id synset1) (synset-id synset2)))

(defun lookup-synset (synset-id &key (pos-db *pos-db*))
  (gethash synset-id (pos-synset-table pos-db)))

(defun synsets (word &key pos (pos-db *pos-db*))
  (cond ((null pos)
         (gethash word (pos-word-to-synset-table pos-db)))
        ((symbolp pos)
         (gethash (cons word (wordnet-pos pos))
                  (pos-word-pos-to-synset-table pos-db)))
        ((integerp pos)
         (gethash (cons word pos)
                  (pos-word-pos-to-synset-table pos-db)))))

(defun synset-type-to-wordnet-pos (synset-type)
  (cond ((equal synset-type "n") +noun+)
        ((equal synset-type "v") +verb+)
        ((equal synset-type "a") +adjective+)
        ((equal synset-type "s") +satellite+)
        ((equal synset-type "r") +adverb+)))

(defun synset-type-to-pos (synset-type)
  (cond ((equal synset-type "n") 'NN)
        ((equal synset-type "v") 'VB)
        ((equal synset-type "a") 'JJ)
        ((equal synset-type "s") 'JJ)
        ((equal synset-type "r") 'RB)))

(defun wordnet-pos (tag)
  "Translate a PennTreebank POS tag into a Wordnet POS id"
  (case tag
    ((NN NNS NNP NNPS)    cffi-wordnet:+noun+)
    ((VB VBD VBG VBN VBZ) cffi-wordnet:+verb+)
    ((JJ JJR JJS)         cffi-wordnet:+adjective+)
    ((RB RBR RBS)         cffi-wordnet:+adverb+)
    (otherwise            nil)))

(defun make-synset (&key synset-id synset-type sense-number tag-count (pos-db *pos-db*))
  (assert (member synset-type '("n" "v" "a" "s" "r") :test 'string=))
  (let ((synset (make-instance 'synset
                               :synset-id synset-id
                               :synset-type synset-type
                               :sense-number sense-number
                               :tag-count tag-count)))
    (setf (gethash synset-id (pos-synset-table pos-db)) synset)))

(defmethod add-word-to-synset ((synset synset) (word string) (word-number integer) &key (pos-db *pos-db*))
  (unless (>= (array-dimension (synset-words synset) 0)
              word-number)
    (adjust-array (synset-words synset) (list word-number)))
  (setf (aref (synset-words synset) (1- word-number)) word)
  (pushnew synset
           (gethash word (pos-word-to-synset-table pos-db))
           :test 'synset-eq)
  (pushnew synset (gethash (list word
                                 (synset-type-to-wordnet-pos
                                  (synset-type synset)))
                           (pos-word-pos-to-synset-table pos-db))
           :test 'synset-eq)
  (pushnew synset (gethash (list word (sense-number synset))
                           (pos-word-sense-to-synset-table pos-db))
           :test 'synset-eq)
  synset)

(defun read-wordnet-synsets (&key (pos-db *pos-db*) (path "prolog") (file "wn_s.pl"))
  (with-open-file (in (format nil "~A/~A" path file))
    (clrhash (pos-synset-table pos-db))
    (clrhash (pos-word-to-synset-table pos-db))
    (clrhash (pos-word-sense-to-synset-table pos-db))
    (clrhash (pos-word-pos-to-synset-table pos-db))
    (let ((count 0))
      (do ((line (read-line in nil :eof) (read-line in nil :eof)))
          ((eql line :eof))
        (when (> (length line) 0)
          (incf count)
          (cl-ppcre:do-register-groups (id word-number word ss-type sense-number tag-count)
              ("^s\\(([0-9]+)\,([0-9]+)\,\'(.*)\'\,([nvasr]{1})\,([0-9]+)\,([0-9]+)\\)\\.$"
               line)
            (let ((synset-id (parse-integer id))
                  (word-number (parse-integer word-number)))
              (let ((synset (or (lookup-synset synset-id)
                                (make-synset :synset-id synset-id
                                             :synset-type ss-type
                                             :sense-number sense-number
                                             :tag-count tag-count
                                             :pos-db pos-db))))
                (add-word-to-synset synset word word-number))))))
      count)))

(defun read-glosses (&key (pos-db *pos-db*) (path "prolog") (file "wn_g.pl"))
  (with-open-file (in (format nil "~A/~A" path file))
    (let ((count 0))
      (do ((line (read-line in nil :eof) (read-line in nil :eof)))
          ((eql line :eof))
        (when (> (length line) 0)
          (incf count)
          (cl-ppcre:do-register-groups (ss-id gloss)
              ("^g\\(([0-9]+)\,\\'(.*)\\'\\)\\.$"
               line)
            (let ((synset-id (parse-integer ss-id)))
              (let ((synset (lookup-synset synset-id :pos-db pos-db)))
                (if (not (synset-p synset))
                    (format t "Line ~A: Invalid synset id ~A~%" count synset-id)
                    (setf (synset-gloss synset) gloss)))))))
      count)))

(defun read-wordnet-pointerfile (file prolog-op slot-name &key reverse-links (pos-db *pos-db*))
  (with-open-file (in file)
    (let ((count 0)
          (regex (format nil "^~A\\(([0-9]+)\,([0-9]+)\\)\\.$" prolog-op)))
      (do ((line (read-line in nil :eof) (read-line in nil :eof)))
          ((eql line :eof))
        (when (> (length line) 0)
          (incf count)
          (cl-ppcre:do-register-groups (ss-id-1 ss-id-2)
              (regex line)
            (let ((synset-id-1 (parse-integer ss-id-1))
                  (synset-id-2 (parse-integer ss-id-2)))
              (let ((synset-1 (lookup-synset synset-id-1 :pos-db pos-db))
                    (synset-2 (lookup-synset synset-id-2 :pos-db pos-db)))
                (when (not (synset-p synset-1))
                  (error "Line ~A: Invalid synset id ~A" count synset-id-1))
                (when (not (synset-p synset-2))
                  (error "Line ~A: Invalid synset id ~A" count synset-id-2))
                (if reverse-links
                    (pushnew synset-1 (slot-value synset-2 slot-name))
                    (pushnew synset-2 (slot-value synset-1 slot-name))))))))
      count)))

(defmethod synset-word-list ((synset synset))
  (map 'list 'identity (synset-words synset)))

(defmethod convert-pos-to-wordnet (pos)
  (typecase pos
    (symbol (wordnet-pos pos))
    (integer pos)
    (otherwise (error "Unknown part of speech: ~A" pos))))

(defun synonyms (word &key pos (pos-db *pos-db*) (return-type :synsets))
  (let ((synsets
         (if pos
             (gethash (list word (convert-pos-to-wordnet pos))
                      (pos-word-pos-to-synset-table pos-db))
             (gethash word (pos-word-to-synset-table pos-db)))))
    (if (eql return-type :words)
        (delete-duplicates (mapcan 'synset-word-list synsets) :test 'string=)
        synsets)))

(defmethod synonym-p ((word1 string) (word2 string) &key pos (pos-db *pos-db*))
  (intersection (synonyms word1 :return-type :words :pos pos :pos-db pos-db)
                (synonyms word2 :return-type :words :pos pos :pos-db pos-db)))

(defmethod hypernyms ((synset synset) &key &allow-other-keys)
  (map 'list 'identity (hypernym-ptrs synset)))

(defmethod hypernyms ((word string) &key pos (pos-db *pos-db*) &allow-other-keys)
  (let ((synsets (if pos
                     (gethash (list word (convert-pos-to-wordnet pos))
                              (pos-word-pos-to-synset-table pos-db))
                     (gethash word (pos-word-to-synset-table pos-db)))))
    (union-all (mapcar 'hypernym-ptrs synsets))))

(defmethod hyponyms ((synset synset) &key &allow-other-keys)
  (map 'list 'identity (hyponym-ptrs synset)))

(defmethod hyponyms ((word string) &key pos (pos-db *pos-db*) &allow-other-keys)
  (let ((synsets (if pos
                     (gethash (list word (convert-pos-to-wordnet pos))
                              (pos-word-pos-to-synset-table pos-db))
                     (gethash word (pos-word-to-synset-table pos-db)))))
    (union-all (mapcar 'hyponym-ptrs synsets))))

(defmethod make-meronyms ((synset synset) &key &allow-other-keys)
  (union (union (map 'list 'identity (member-meronym-ptrs synset))
                (map 'list 'identity (substance-meronym-ptrs synset)))
         (map 'list 'identity (part-meronym-ptrs synset))))

(defmethod meronyms ((synset synset) &key &allow-other-keys)
  (meronym-ptrs synset))

(defmethod meronyms ((word string) &key pos (pos-db *pos-db*) &allow-other-keys)
  (let ((synsets (if pos
                     (gethash (list word (convert-pos-to-wordnet pos))
                              (pos-word-pos-to-synset-table pos-db))
                     (gethash word (pos-word-to-synset-table pos-db)))))
    (union-all (mapcar 'meronym-ptrs synsets))))

(defmethod make-holonyms ((synset synset) &key &allow-other-keys)
  (union (union (map 'list 'identity (member-holonym-ptrs synset))
                (map 'list 'identity (substance-holonym-ptrs synset)))
         (map 'list 'identity (part-holonym-ptrs synset))))

(defmethod holonyms ((synset synset) &key &allow-other-keys)
  (holonym-ptrs synset))

(defmethod holonyms ((word string) &key pos (pos-db *pos-db*) &allow-other-keys)
  (let ((synsets (if pos
                     (gethash (list word (convert-pos-to-wordnet pos))
                              (pos-word-pos-to-synset-table pos-db))
                     (gethash word (pos-word-to-synset-table pos-db)))))
    (union-all (mapcar 'holonym-ptrs synsets))))

(defun instances (word &key pos (pos-db *pos-db*))
  (let ((synsets (if pos
                     (gethash (list word (convert-pos-to-wordnet pos))
                              (pos-word-pos-to-synset-table pos-db))
                     (gethash word (pos-word-to-synset-table pos-db)))))
    (union-all (mapcar 'instance-ptrs synsets))))

(defmethod semantic-neighborhood ((word string) &key pos (pos-db *pos-db*))
  (union-all (mapcar 'semantic-neighborhood-ptrs (synonyms word :pos pos :pos-db pos-db))))

(defmethod semantic-neighborhood ((synset synset) &key &allow-other-keys)
  (semantic-neighborhood-ptrs synset))

(defmethod make-semantic-neighborhood ((synset synset) &key (pos-db *pos-db*) &allow-other-keys)
  (union-all (list (instances synset :pos-db pos-db)
                   (meronyms synset :pos-db pos-db)
                   (holonyms synset :pos-db pos-db)
                   (hyponyms synset :pos-db pos-db)
                   (hypernyms synset :pos-db pos-db))))

(defmethod make-semantic-parents ((synset synset) &key &allow-other-keys)
  (union (holonyms synset)
         (hypernyms synset)))

(defmethod semantic-parents ((word string) &key pos (pos-db *pos-db*) &allow-other-keys)
  (let ((synsets (if pos
                     (gethash (list word (convert-pos-to-wordnet pos))
                              (pos-word-pos-to-synset-table pos-db))
                     (gethash word (pos-word-to-synset-table pos-db)))))
    (union-all (mapcar 'semantic-parent-ptrs synsets))))

(defmethod semantic-parents ((synset synset) &key &allow-other-keys)
  (semantic-parent-ptrs synset))

(defmethod make-semantic-children ((synset synset) &key &allow-other-keys)
  (union (meronyms synset)
         (hyponyms synset)))

(defmethod semantic-children ((word string) &key pos (pos-db *pos-db*) &allow-other-keys)
  (let ((synsets (if pos
                     (gethash (list word (convert-pos-to-wordnet pos))
                              (pos-word-pos-to-synset-table pos-db))
                     (gethash word (pos-word-to-synset-table pos-db)))))
    (union-all (mapcar 'semantic-child-ptrs synsets))))

(defmethod semantic-children ((synset synset) &key &allow-other-keys)
  (semantic-child-ptrs synset))

(defun populate-wordnet-database (&key (pos-db *pos-db*) (path "prolog"))
  (read-wordnet-synsets :pos-db pos-db :path path)
  (read-glosses :pos-db pos-db :path path)
  (read-wordnet-pointerfile (format nil "~A/wn_hyp.pl" path) "hyp" 'hypernym-ptrs :pos-db pos-db)
  (read-wordnet-pointerfile (format nil "~A/wn_hyp.pl" path)
                            "hyp"
                            'hyponym-ptrs
                            :reverse-links t
                            :pos-db pos-db)
  (read-wordnet-pointerfile (format nil "~A/wn_ins.pl" path) "ins" 'instance-ptrs :pos-db pos-db)

  (read-wordnet-pointerfile (format nil "~A/wn_mm.pl" path)
                            "mm"
                            'member-meronym-ptrs
                            :reverse-links t
                            :pos-db pos-db)
  (read-wordnet-pointerfile (format nil "~A/wn_ms.pl" path)
                            "ms"
                            'substance-meronym-ptrs
                            :reverse-links t
                            :pos-db pos-db)
  (read-wordnet-pointerfile (format nil "~A/wn_mp.pl" path)
                            "mp"
                            'part-meronym-ptrs
                            :reverse-links t
                            :pos-db pos-db)
  (read-wordnet-pointerfile (format nil "~A/wn_mm.pl" path) "mm" 'member-holonym-ptrs :pos-db pos-db)
  (read-wordnet-pointerfile (format nil "~A/wn_ms.pl" path) "ms" 'substance-holonym-ptrs :pos-db pos-db)
  (read-wordnet-pointerfile (format nil "~A/wn_mp.pl" path) "mp" 'part-holonym-ptrs :pos-db pos-db)
  ;; Calculate important aggregations
  (maphash (lambda (id synset)
             (declare (ignore id))
             (setf (semantic-neighborhood-ptrs synset)
                   (make-semantic-neighborhood synset)
                   (meronym-ptrs synset)
                   (make-meronyms synset)
                   (holonym-ptrs synset)
                   (make-holonyms synset)
                   (semantic-parent-ptrs synset)
                   (make-semantic-parents synset)
                   (semantic-child-ptrs synset)
                   (make-semantic-children synset)))
           (pos-synset-table pos-db))
  (pos-synset-table pos-db))
