(in-package #:nlp)

(defvar *corpus* (make-hash-table))

(defun normalize (text)
  (cl-ppcre:regex-replace-all
   "\\s+"
   (cl-ppcre:regex-replace-all
    "[^a-z']+$"
    (cl-ppcre:regex-replace-all
     "([^a-z'-.\\s]|\'\')|\,"
     (cl-ppcre:regex-replace-all
      "\/[^\\s]+"
      (string-downcase text)
      " ")
     "")
    "")
   " "))

(defun load-corpus (file)
  "Compute unigrams and bigrams for a corpus"
  (let ((corpus (make-hash-table))
	(total-word-count 0)
	(word-count (make-hash-table :test 'equalp))
	(g1 (make-hash-table :test 'equalp))
	(g2 (make-hash-table :test 'equalp)))
    (with-open-file (in file)
      (do ((line (read-line in nil :eof) (read-line in nil :eof)))
	  ((or (null line) (eql line :eof)))
	(let ((words (append (cl-ppcre:split "\\s+" (normalize line))
			     (list "</s>")))
	      (w1 "<s>"))
	  (setq total-word-count (+ total-word-count (length words)))
	  (init-or-increment word-count (list w1))
	  (init-or-increment g1 (list w1))
	  (dotimes (i (length words))
	    (let ((word (elt words i)))
	      (init-or-increment word-count word)
	      (init-or-increment g1 (list word))
	      (init-or-increment g2 (list w1 word))
	      (setq w1 word))))))
    ;; Compute probabilities
    (setf (gethash 1 corpus) (make-hash-table :test 'equalp)
	  (gethash 2 corpus) (make-hash-table :test 'equalp))
    (maphash (lambda (ngram count)
               (setf (gethash ngram (gethash 1 corpus))
                     (/ count total-word-count)))
	     g1)
    (maphash (lambda (ngram count)
               (setf (gethash ngram (gethash 2 corpus))
                     (/ count (or (gethash (butlast ngram) g1)
                                  total-word-count))))
	     g2)
    (setq *corpus* corpus)))

(defun dump-corpus (&optional (corpus *corpus*))
  (maphash (lambda (n table)
             (format t "~A-gram~%" n)
             (maphash (lambda (ngram count)
                        (format t "  ~A: ~A~%" ngram count))
                      table))
	   corpus))

(defun corpus-summary (&optional (corpus *corpus*))
  (let ((unigrams nil) (bigrams nil))
    (maphash (lambda (ngram p)
               (push (cons ngram p) unigrams))
	     (gethash 1 corpus))
    (maphash (lambda (ngram p)
               (push (cons ngram p) bigrams))
	     (gethash 2 corpus))
    (values
     (subseq (sort unigrams '> :key 'cdr) 0 20))))
     ;;(sort bigrams 'string< :key 'caar))))

(defun kl-divergence (corpus1 corpus2 &key (n 1))
  "Compute the Kullback–Leibler divergence between to corpora"
  (let ((divergence 0))
    (maphash (lambda (word p)
               (setq divergence
                     (+ divergence
                        (handler-case
                            (* p
                               (log (/ p
                                       (gethash word
                                                (gethash n corpus2)))))
                          (error (c)
                            (declare (ignore c))
                            0)))))
	     (gethash n corpus1))
    divergence))
