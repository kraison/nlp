(in-package #:nlp)

(defun translate-moby-to-penn (pos word)
  "Translate a Moby POS tag to a Penn tag"
  (case pos
    (#\N (if (upper-case-p (elt word 0)) 'NNP 'NN))
    (#\p (if (upper-case-p (elt word 0)) 'NNPS 'NNS))
    (#\h 'NP)
    (#\V 'VBG) ;; Not so sure about the verbs...
    (#\t 'VB)
    (#\i 'VB)
    (#\A 'JJ)
    (#\v 'RB)
    (#\C 'CC)
    (#\P 'IN)
    (#\! 'UH)
    (#\r 'PRP)
    (#\D 'DT)
    (#\I 'DT)
    (#\o  nil)
    (:otherwise nil)))

(defun augment-lexicon (file lexicon counts frequencies &key type)
  "Augment our learned lexicon with Moby's POS lexicon or a user-supplied lexicon using Penn tags."
  (with-open-file (stream file :element-type 'character)
    (do ((line (read-line stream nil :eof) (read-line stream nil :eof)))
	((eq line :eof))
      (when (> (length line) 1)
	(destructuring-bind (word pos) (split "\\\\" line :limit 2)
          (let ((pos-list
                 (case type
                   (:moby
                    (remove-if (lambda (p) (or (null p) (eq 'NP p)))
                               (remove-duplicates
                                (map 'list (lambda (p)
                                             (translate-moby-to-penn p word))
                                     pos))))
                   (:penn
                    (mapcar (lambda (p) (intern p :nlp)) (split "\\s+" pos))))))
	    (incf (gethash word frequencies 0))
	    (dolist (p pos-list)
	      ;;(format t "~A: ~A / ~A~%" word pos p)
	      (if (gethash word lexicon)
		  (pushnew p (gethash word lexicon) :test 'equal)
		  (setf (gethash word lexicon) (list p)))
	      (unless (hash-table-p (gethash word counts))
		(setf (gethash word counts) (make-hash-table)))
	      (setf (gethash p (gethash word counts))
		    (1+ (gethash p (gethash word counts) 1))))))))))

(defun make-lexicon (infile &key (equality 'equalp) outfile moby-file user-file)
  "Derive a lexicon from a tagged corpus.  Also accepts a Moby parts-of-speech file
as an optional argument;  will translate the Moby data into Penn tags and add to the
lexicon.  Also takes an additional user-file with either Moby or Penn style tags."
  (let ((lexicon (make-hash-table :test equality))
	(p-lexicon (make-hash-table :test equality))
	(counts (make-hash-table :test equality))
	(frequencies (make-hash-table :test 'equal)))
    (map-tagged-corpus
     (lambda (word pos)
       (setf (gethash word frequencies)
             (1+ (gethash word frequencies 1)))
       (if (gethash word lexicon)
           (pushnew pos (gethash word lexicon) :test 'equal)
           (setf (gethash word lexicon) (list pos)))
       (unless (hash-table-p (gethash word counts))
         (setf (gethash word counts) (make-hash-table)))
       (setf (gethash pos (gethash word counts))
             (1+ (gethash pos (gethash word counts) 1))))
     infile)
    (when (and moby-file (probe-file moby-file))
      (augment-lexicon moby-file lexicon counts frequencies :type :moby))
    (when (and user-file (probe-file user-file))
      (augment-lexicon user-file lexicon counts frequencies :type :penn))
    (maphash
     (lambda (word table)
       (let ((total (loop
                       for pos being the hash-keys of table
                       using (hash-value c)
                       summing c into total
                       finally (return total))))
         (maphash (lambda (pos count)
                    (push (list pos (/ count total))
                          (gethash word p-lexicon)))
                  table)))
     counts)
    (when outfile
      (with-open-file (out outfile :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
	(maphash (lambda (word p)
                   (format out "~A ~{~A~^ ~}~%" word p))
		 lexicon)))
    (values lexicon p-lexicon frequencies)))

(defun load-lexicon (file &key (equality 'equal))
  "Load a lexicon file"
  (let ((lexicon (make-hash-table :test equality)))
    (with-open-file (in file)
      (do ((line (read-line in nil :eof) (read-line in nil :eof)))
	  ((or (null line) (eql line :eof)))
	(let ((list (cl-ppcre:split "\\s+" line)))
	  (setf (gethash (first list) lexicon)
		(mapcar 'intern (rest list))))))
    lexicon))

(defun map-lexicon (fn &optional (pos-db *pos-db*))
  (maphash fn (pos-lexicon pos-db)))

(let ((number-regex (create-scanner "^[0-9./,:]+\\%?$" :single-line-mode t))
      (verb-regex (create-scanner "\\w(ing|ate|ify|ize|ise|ed)$"
				  :single-line-mode t :case-insensitive-mode t))
      (adj-regex (create-scanner "\\w(able|ible|al|ial|ic|y|ing|ed|ful|ish|ive|ous|ious)$"
				 :single-line-mode t :case-insensitive-mode t))
      (adv-regex (create-scanner "\\w(ly|ally|ily)$"
				 :single-line-mode t :case-insensitive-mode t))
      (noun-regex
       (create-scanner
	"\\w(er|or|ance|ence|ant|ent|ee|ess|ian|ism|ics|ist|ity|ment|ness|ship|tion|ation|ure|man|woman|eur|ing|hood)$"
	:single-line-mode t :case-insensitive-mode t))
      (pnoun-regex (create-scanner "^[A-Z]\\w" :single-line-mode t :case-insensitive-mode nil)))
  (defun in-lexicon? (pos-db word pos)
    "Is word as pos in the lexicon?"
    (when (symbolp word) (setq word (symbol-name word)))
    (or (member pos (gethash word (pos-lexicon pos-db)) :test 'equal)
	(member pos (gethash (string-downcase word) (pos-lexicon pos-db)) :test 'equal)
	(and (eq pos 'CD) (scan number-regex word))))

  (defun lookup-pos (word &optional (pos-db *pos-db*))
    "Return all possible parts of speech for word"
    (let ((pos-list (gethash word (pos-lexicon pos-db))))
      (when (scan number-regex word)
	(pushnew 'CD pos-list))
      (remove-duplicates
       (remove-if
        'null
        (append
         (or pos-list
             (append
              (and (scan noun-regex word)  '(NN))
              (and (scan pnoun-regex word) '(NNP))
              (and (scan verb-regex word)  '(VB))
              (and (scan adv-regex word)   '(RB))
              (and (scan adj-regex word)   '(JJ))))
         (mapcan (lambda (pair)
                   (and (scan (car pair) word) (list (cdr pair))))
                 (pos-user-pos-regex pos-db)))))))

  (defun is-a (word pos &optional (pos-db *pos-db*))
    (member pos (lookup-pos word pos-db))))

(defun add-to-lexicon (word pos &optional (pos-db *pos-db*))
  "Add a word to the lexicon"
  (pushnew pos (gethash word (pos-lexicon pos-db)) :test 'eq))

(defun add-pos-regex (regex pos &optional (pos-db *pos-db*))
  (push (cons (create-scanner regex :single-line-mode t :case-insensitive-mode t)
              pos)
        (pos-user-pos-regex pos-db)))

(defun get-pos-probabilities (word)
  "Parts of speech with learned probabilities for WORD"
  (gethash word (pos-plexicon *pos-db*)))

(defun get-lexical-pos-probability (word)
  )

;; Spelling corrector based on Norvig's:
;; http://mikael.jansson.be/log/spellcheck-in-lisp
(defvar *alphabet* "abcdefghijklmnopqrstuvwxyz0123456789-'/")

(defun edits-1 (word)
  "Find edits of one character"
  (let* ((splits (loop for i from 0 upto (length word)
		    collecting (cons (subseq word 0 i) (subseq word i))))
         (deletes (loop for (a . b) in splits
		     when (not (zerop (length b)))
		     collect (concatenate 'string a (subseq b 1))))
         (transposes (loop for (a . b) in splits
			when (> (length b) 1)
			collect (concatenate 'string a (subseq b 1 2)
                                             (subseq b 0 1) (subseq b 2))))
         (replaces (loop for (a . b) in splits
		      nconcing (loop for c across *alphabet*
				  when (not (zerop (length b)))
				  collect (concatenate 'string a (string c)
                                                       (subseq b 1)))))
         (inserts (loop for (a . b) in splits
		     nconcing (loop for c across *alphabet*
				 collect (concatenate 'string a (string c) b)))))
    (remove-if (lambda (w1)
                 (> (edit-distance word w1) 1))
	       (remove-duplicates
		(nconc deletes transposes replaces inserts)
		:test 'equal))))

(defun known-edits-2 (word)
  "Find edits of 2 characters"
  (remove-duplicates
   (loop for e1 in (edits-1 word) nconcing
	(loop for e2 in (edits-1 e1)
	   when (multiple-value-bind (value pp)
		    (gethash e2 (pos-word-freq *pos-db*) 1)
		  (declare (ignore value))
		  pp)
	   collect e2))
   :test 'equal))

(defun known (words)
  "Remove unknown words from list"
  (loop for word in words
     when (multiple-value-bind (value pp)
	      (gethash word (pos-word-freq *pos-db*) 1)
	    (declare (ignore value))
	    pp)
     collect word))

(defun correct-spelling (word)
  "Correct spelling for a single word"
  (let ((winner word) (max-f 0))
    (dolist (n-word (or (known (list word))
                        (known (edits-1 word))
                        (known-edits-2 word)
                        (list word)))
      (let ((f (gethash n-word (pos-word-freq *pos-db*) 1)))
	;;(format t "Trying word ~A: F -> ~D~%" n-word f)
	(when (> f max-f)
	  (setq winner n-word max-f f))))
    winner))

(defun spell-check (text &key join?)
  "Correct spelling in arbitrary text"
  (let ((words (mapcar (lambda (word)
                         (loop for word in (or (known (list word))
                                               (known (edits-1 word))
                                               (known-edits-2 word)
                                               (list word))
                            maximizing (gethash word (pos-word-freq *pos-db*) 1)
                            finally (return word)))
		       (tokenize text))))
    (if join?
	(if (member (last1 words) '(#\. #\? #\!))
	    (format nil "~{~A~^ ~}~A" (butlast words) (last1 words))
	    (format nil "~{~A~^ ~}" words))
	words)))

(defun get-synonyms (word &key (include-original? t))
  "Use Wordnet to get synonyms for all senses of the given word"
  (let ((synonyms
         (remove-duplicates
          (mapcan (lambda (tag)
                    (let ((wordnet-pos (wordnet-pos tag)))
                      (when wordnet-pos
                        (wordnet-search word
                                        :part-of-speech wordnet-pos
                                        :sense +all-senses+
                                        :search-type +synonyms+))))
                  (lookup-pos word))
          :test 'equalp)))
    (if include-original?
        (nconc (list word) synonyms)
        synonyms)))

(defun get-hypernyms (word &key pos (include-original? nil))
  "Use Wordnet to get hypernyms for all senses of the given word"
  (let ((hypernyms
         (mapcar (lambda (tag)
                   (let ((wordnet-pos (wordnet-pos tag)))
                     (when wordnet-pos
                       (list tag
                             (wordnet-search word
                                             :part-of-speech wordnet-pos
                                             :sense +all-senses+
                                             :search-type cffi-wordnet::HYPERPTR)))))
                 (or (if pos (list pos) (lookup-pos word))))))
    (if include-original?
        (push (list word) hypernyms)
        hypernyms)))

(defun get-meronyms (word &key (include-original? t))
  "Use Wordnet to get synonyms for all senses of the given word"
  (let ((meronyms
         (remove-duplicates
          (mapcan (lambda (tag)
                    (let ((wordnet-pos (wordnet-pos tag)))
                      (when wordnet-pos
                        (wordnet-search word
                                        :part-of-speech wordnet-pos
                                        :sense +all-senses+
                                        :search-type +meronym+))))
                  (lookup-pos word))
          :test 'equalp)))
    (if include-original?
        (nconc (list word) meronyms)
        meronyms)))

(defun get-holonyms (word &key (include-original? t))
  "Use Wordnet to get synonyms for all senses of the given word"
  (let ((holonyms
         (remove-duplicates
          (mapcan (lambda (tag)
                    (let ((wordnet-pos (wordnet-pos tag)))
                      (when wordnet-pos
                        (wordnet-search word
                                        :part-of-speech wordnet-pos
                                        :sense +all-senses+
                                        :search-type cffi-wordnet::HHOLONYM))))
                  (lookup-pos word))
          :test 'equalp)))
    (if include-original?
        (nconc (list word) holonyms)
        holonyms)))

(defun wordnet-pos (tag)
  "Translate a PennTreebank POS tag into a Wordnet POS id"
  (case tag
    ((NN NNS NNP NNPS)    +noun+)
    ((VB VBD VBG VBN VBZ) +verb+)
    ((JJ JJR JJS)         +adjective+)
    ((RB RBR RBS)         +adverb+)
    (:otherwise           nil)))
