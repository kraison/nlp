(ql:quickload "nlp")

(in-package :nlp)

(defun test-grammar (&optional
		     (file "~/work/school/csc599/data/all-parsed.txt"))
  (let ((grammar (extract-grammar-rules file)))
;;    (maphash #'(lambda (lhs rhs)
;;		 (format t "~A -> ~{~A~^|~}~%" lhs rhs)
;;		 (unless (y-or-n-p "Continue? ")
;;		   (return-from test-grammar grammar)))
;;	     grammar)
    grammar))

(defun write-grammar (grammar &optional (file "grammar.txt"))
  (with-open-file (out file :direction :output :if-exists :supersede)
    (maphash #'(lambda (lhs rhs)
		 (format out "~A -> " lhs)
		 (if (every #'stringp rhs)
		     (format out "~{~A~^|~}~%" rhs)
		     (loop for i from 0 to (1- (length rhs)) do
			  (format out "~{~A~^ ~}" rhs)
			  (if (< i (1- (length rhs)))
			      (format out "|")
			      (format out "~%")))))
	     grammar)))

(defun parse-test (&key
		   (grammar-train "data/all-parsed.txt")
		   (pos-train "data/all-pos.txt"))
  (format t "Training...~%")
  (setq *pos-db* (train-tagger pos-train)) ;; pos-lex)
  (extract-grammar-rules grammar-train *pos-db*)
  (make-cnf-grammar (pos-cfg *pos-db*) *pos-db*)
  (cyk-parse "He went to his mother's house in Maine for the summer."))

(ql:quickload "trivial-timeout")
(defun tomuro-earleycfg (&optional (corpus "data/all-parsed.txt"))
  ;;(load-pcfg "data/p-grammar.txt" *pos-db*)
  (with-open-file (p-log "earley-tomuro.log"
			 :direction :output
			 :if-exists :supersede)
    (map-parsed-corpus
     (lambda (tree)
       (let ((words (syntax-leaves tree)))
         (format t "~A~%" words)
         (format p-log "~A~%" words)
         (multiple-value-bind (parse tags)
             (handler-case
                 (trivial-timeout:with-timeout (10)
                   (earley-parse (butlast words)))
               (error (c)
                 (declare (ignore c))
                 (sb-ext:gc :full t)
                 (values nil (tag-sentence words))))
           (format t "~A~%~A~%~%" tags parse)
           (format p-log "~A~%~A~%~%" tags parse))))
     corpus)))

(defun tomuro-cfg (&optional (corpus "data/all-parsed.txt"))
  ;;(load-cfg "data/grammar.txt" *pos-db*)
  (make-pcfg-from-cfg "data/p-grammar.txt" *pos-db*)
  (with-open-file (log "tomuro.log"
		       :direction :output
		       :if-exists :supersede)
    (with-open-file (p-log "p-tomuro.log"
			   :direction :output
			   :if-exists :supersede)
      (map-parsed-corpus
       (lambda (tree)
         (let ((words (syntax-leaves tree)))
           (format t "~A~%" words)
           (format log "~A~%" words)
           (format p-log "~A~%" words)
           (multiple-value-bind (parse tags)
               (handler-case
                   (trivial-timeout:with-timeout (10)
                     (chart-parse (butlast words)))
                 (error (c)
                   (declare (ignore c))
                   (sb-ext:gc :full t)
                   (values nil (tag words))))
             (format t "~A~%~A~%~%" tags parse)
             (format log "~A~%~A~%~%" tags parse))
           (multiple-value-bind (parse tags)
               (handler-case
                   (trivial-timeout:with-timeout (10)
                     (pcp-parse (butlast words)))
                 (error (c)
                   (declare (ignore c))
                   (sb-ext:gc :full t)
                   (values nil (tag words))))
             (format t "~A~%~A~%~%" tags parse)
             (format p-log "~A~%~A~%~%" tags parse))))
       corpus))))
