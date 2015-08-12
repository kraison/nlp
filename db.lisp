(in-package :nlp)

(defun print-pos (pos stream depth)
  (declare (ignore depth))
  (format
   stream
   "#<PARTS-OF-SPEECH: ~A bigrams, ~A trigrams ~A observation likelihoods>"
   (hash-table-count (pos-bigrams pos))
   (hash-table-count (pos-trigrams pos))
   (hash-table-count (pos-observations pos))))

(defstruct (parts-of-speech
	     (:predicate pos-db?)
	     (:conc-name pos-)
	     (:print-function print-pos))
  ;; POS tagging
  (total-count 0)
  (gammas (make-hash-table))
  (probabilities (make-hash-table :test 'equal))
  (unigrams (make-hash-table :test 'equal))
  (bigrams (make-hash-table :test 'equal))
  (trigrams (make-hash-table :test 'equal))
  (tag-occurances (make-hash-table :test 'equal))
  (word-occurances (make-hash-table :test 'equal))
  (observations (make-hash-table :test 'equal))
  (unknown-probability 0)

  ;; HMM Chunking
  (np-total-count 0)
  (np-gammas (make-hash-table))
  (np-probabilities (make-hash-table :test 'equal))
  (np-unigrams (make-hash-table :test 'equal))
  (np-bigrams (make-hash-table :test 'equal))
  (np-trigrams (make-hash-table :test 'equal))
  (np-tag-occurances (make-hash-table :test 'equal))
  (np-pos-occurances (make-hash-table :test 'equal))
  (np-observations (make-hash-table :test 'equal))
  (np-unknown-probability 0)

  ;; Lexicon
  (lexicon (make-hash-table :test 'equal))
  (plexicon (make-hash-table :test 'equal))
  (user-pos-regex nil)
  (contraction-table (make-hash-table :test 'equalp))
  (word-freq (make-hash-table :test 'equal))

  ;; Parsing
  (np-regexes nil)
  (cfg (make-hash-table))
  (cfg-idx (make-hash-table))
  (pcfg (make-hash-table :test 'equalp))
  (lcfg nil) ;;(make-lcfg-table))
  (cnf-grammar (make-hash-table))
  (cnf-index (make-hash-table :test 'equalp))
  (cnf-subs-map (make-hash-table :test 'equalp))
  (cnf-subs-rev (make-hash-table :test 'equalp)))
