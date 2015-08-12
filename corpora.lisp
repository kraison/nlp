(in-package #:nlp)

(defun map-parsed-corpus (fn file &key collect?)
  "Apply fn to each tree in parsed corpus FILE.
This function assumes that all sensitive characters have been escaped:
s/[.,`':;#|]{1}/\\$1/g"
  (let ((result nil))
    (let ((*readtable* (copy-readtable)))
      (setf (readtable-case *readtable*) :preserve)
      ;;(set-macro-character #\, (lambda (stream char) '|,|) nil *readtable*)
      ;;(set-macro-character #\. (lambda (stream char) '|.|) nil *readtable*)
      ;;(set-macro-character #\` (lambda (stream char) '|`|) nil *readtable*)
      ;;(set-macro-character #\' (lambda (stream char) '|'|) nil *readtable*)
      ;;(set-macro-character #\" (lambda (stream char) '|"|) nil *readtable*)
      (with-open-file (in file)
	(do ((tree (read in nil :eof) (read in nil :eof)))
	    ((eql tree :eof))
	  (if collect?
	      (push (funcall fn (first tree)) result)
	      (funcall fn (first tree))))))
    (nreverse result)))

(defun read-tagged-word (stream)
  "Read a tagged word from the stream"
  (let ((word (make-array 0
			  :element-type 'character
			  :fill-pointer t
			  :adjustable t)))
    (peek-char t stream nil :eof)
    (do ((c (read-char stream nil :eof) (read-char stream nil :eof)))
	((or (eql c :eof) (member c *whitespace*)))
      (vector-push-extend c word))
    (when (> (length word) 0)
      word)))

(defun tagged-split (word)
  "Split a tagged word into word and POS tag"
  (let ((position (position #\/ word :from-end t)))
    (if (numberp position)
	(let ((pos (remove #\^ (subseq word (1+ position)))))
	  (values (subseq word 0 position)
		  (intern pos)))
	(values word nil))))

(defun map-tagged-corpus (fn file &key collect?)
  "Apply fn to each tagged word in FILE"
  (let ((result nil))
    (with-open-file (in file)
      (do ((word (read-tagged-word in) (read-tagged-word in)))
	  ((null word))
	(multiple-value-bind (word pos) (tagged-split word)
	  (when (and word pos)
	    (if collect?
		(push (funcall fn word pos) result)
		(funcall fn word pos))))))
    (nreverse result)))

(defun extract-tagged-sentences (file)
  "Break a tagged file into individual sentences"
  (let ((sentences nil)
	(sentence nil)
	(pos-seqs nil)
	(pos-seq nil))
    (map-tagged-corpus
     (lambda (word pos)
       (if (equalp pos '|.|)
           (unless (null sentence)
             (push pos pos-seq)
             (push word sentence)
             (push (nreverse pos-seq) pos-seqs)
             (push (nreverse sentence) sentences)
             (setq sentence nil pos-seq nil))
           (progn
             (push pos pos-seq)
             (push word sentence))))
     file)
    (values (nreverse sentences) (nreverse pos-seqs))))

(defun split-pos-tags (pos-seq)
  (let ((seqs nil))
    (labels ((walk-it (unprocessed-seq processed-seq)
               (if (null unprocessed-seq)
                   (push processed-seq seqs)
                   (let ((pos (first unprocessed-seq)))
                     (if (find #\| (symbol-name pos))
                         (dolist (pos1 (split "\\|" (symbol-name pos)))
                           (walk-it (rest unprocessed-seq)
                                    (append processed-seq (list (intern pos1 :nlp)))))
                         (walk-it (rest unprocessed-seq)
                                  (append processed-seq (list pos))))))))
      (walk-it pos-seq nil))
    seqs))


(defun cleanup-corpus (in-file)
  (with-open-file (out (format nil "~A.new" in-file)
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (let ((sentence nil)
          (pos-seq nil))
      (map-tagged-corpus
       (lambda (word pos)
         (if (equalp pos '|.|)
             (unless (null sentence)
               (push pos pos-seq)
               (push word sentence)
               (setq sentence (nreverse sentence))
               (dolist (new-pos-seq (split-pos-tags (nreverse pos-seq)))
                 (dotimes (i (length new-pos-seq))
                   (format out "~A/~A" (elt sentence i) (elt new-pos-seq i))
                   (if (eql (elt new-pos-seq i) '|.|)
                       (format out "~%")
                       (format out " "))))
               (setq sentence nil pos-seq nil))
             (progn
               (push pos pos-seq)
               (push word sentence))))
       in-file))))
