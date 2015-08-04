(in-package #:nlp)

(defun freeze-nlp (&optional (file "nlp.dat"))
  (cl-store:store *pos-db* file))

(defun thaw-nlp (&optional (file "nlp.dat"))
  (let ((l (cl-store:restore file)))
    (setq *pos-db* l)))

(defun reset-nlp ()
  (setq *pos-db* (make-parts-of-speech)))

(defun init-nlp (&optional file)
  (if file
      (thaw-nlp file)
      (reset-nlp)))

(defmacro maybe-profile ((&body body))
  `(if profile?
       (time (progn ,body))
       (progn
         ,body)))

(defun load-contraction-table (&optional (file "data/contractions.txt") (pos-db *pos-db*))
  (with-open-file (in file)
    (setf (pos-contraction-table pos-db) (make-hash-table :test 'equalp))
    (do ((line (read-line in nil :eof) (read-line in nil :eof)))
        ((eql line :eof))
      (multiple-value-bind (match-p matches)
          (scan-to-strings "^([a-zA-Z']+)\\s+(.*)$" line)
        (when match-p
          (format t "Adding ~A -> ~A~%" (elt matches 0) (split "\\s+" (elt matches 1)))
          (setf (gethash (elt matches 0) (pos-contraction-table pos-db))
                (split "\\s+" (elt matches 1))))))))

(defun build-nlp (&key
                    grammar-train
                    (grammar-load "data/p-grammar.txt")
                    (pos-train "data/all-pos.txt")
                    (pos-lex "data/all-pos.txt")
                    (moby-file "data/moby/mobypos.txt")
                    (contraction-file "data/contractions.txt")
                    user-lexicon-file
                    user-pos-regex
                    save? profile?)
  (flet ((build-it ()
	   (reset-nlp)
	   (format t "Training NLP system...~%")
	   (format t "Building and training lexicon...~%")
           (dolist (pair user-pos-regex)
             (add-pos-regex (car pair) (cdr pair)))
           (load-contraction-table contraction-file *pos-db*)
	   (multiple-value-bind (lex p-lex freq)
               (maybe-profile (make-lexicon pos-lex
                                            :moby-file moby-file
                                            :user-file user-lexicon-file))
	     (setf (pos-lexicon *pos-db*) lex
		   (pos-plexicon *pos-db*) p-lex
		   (pos-word-freq *pos-db*) freq))
	   (format t "Training POS tagger...~%")
           (maybe-profile  (train-tagger pos-train *pos-db*))
	   (format t "Training grammar parser...~%")
	   (if grammar-load
               (maybe-profile
                (load-pcfg grammar-load *pos-db*))
               (maybe-profile (extract-grammar-rules grammar-train *pos-db*)))
	   ;;(format t "Converting CFG to CNF...~%")
           ;;(maybe-profile (make-cnf-grammar (pos-cfg *pos-db*) *pos-db*))
	   (when save?
	     (format t "Freezing POS database...~%")
             (maybe-profile (freeze-nlp)))))
    (maybe-profile (build-it)))
  *pos-db*)
