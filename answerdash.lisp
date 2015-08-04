(ql:quickload :nlp)
(ql:quickload :trivial-shell)

(in-package :nlp)

(defun match? (a b)
  (or (eql a b)
      (let ((a-list (cl-ppcre:split "\\|" (symbol-name a)))
	    (b-list (cl-ppcre:split "\\|" (symbol-name b))))
	(dolist (a a-list)
	  (when (member a b-list :test 'equalp)
	    (return-from match? t)))
	(dolist (b b-list)
	  (when (member b a-list :test 'equalp)
	    (return-from match? t))))))

(defun train (&optional
                (train "data/all-pos.txt")
                (lex "data/all-pos.txt")
                (moby "data/moby/mobypos.txt")
                (user-lexicon-file "data/user-lexicon.txt"))
  (format t "Training...~%")
  (build-nlp :pos-train train
             :pos-lex lex
             :moby-file moby
             :user-lexicon-file user-lexicon-file
             :user-pos-regex
             '(("(http://|https://|www\.)*\\S+\\.(\\w){3,3}(\\S+)*\\b" . NNP)
               ("([A-Z]\\.){3,}" . NNP)
               ("\\w+@\\w+\\.(\\w){3,}\\b" . NNP)
               ("\\$\\d+(\\.\\d{2,2})?" . CD)
               ("\\d+(\\.\\d)?(s|yr|v|ev|vdc|mm|cm|m|m|km|k|l|in|ft|yd|gb|mb|tb|mp|min|hr|kbps|mbps|mah|mhz|g|bit)(\\.)?" . CD)
               ("\\d+x\\d+" . CD)
               ("\\d+%" . CD))))

(defun test (&optional
               (test "data/test-pos.txt"))
  (format t "Tagging...~%")
  (let ((total-words 0)
	(total-correct 0)
	(error-count 0)
	(errors (make-hash-table :test 'equalp)))
    (multiple-value-bind (sentences pos-seqs)
	(extract-tagged-sentences test)
      (format t "Tagging test set.")
      (map nil
           (lambda (sentence pos-seq)
             (format t ".")
             ;;(format t "Tagging '~{~A ~}'~%" sentence)
             (let ((tags (tag-sentence sentence :debug nil)))
               ;;(format t "B: ~{~A ~}~%" pos-seq)
               ;;(format t "K: ~{~A ~}~%~%" tags)
               ;;(when (null (y-or-n-p "Continue? "))
               ;;(return-from test 0))
               (mapcar (lambda (p1 p2)
                         (incf total-words)
                         (if (match? p1 p2)
                             (incf total-correct)
                             (let ((key (cons p1 p2)))
                               (incf error-count)
                               (if (numberp (gethash key errors))
                                   (incf (gethash key errors))
                                   (setf (gethash key errors) 1)))))
                       pos-seq tags)))
           sentences pos-seqs))
    (format t "done~%")
    (let ((error-report nil))
      (maphash (lambda (pair count)
                 (push (list pair count) error-report))
	       errors)
      (format t "Errors:~%")
      (dolist (r (sort error-report '> :key 'second))
	(format t "  ~5A -> ~5A: ~,4F~%"
		(cdr (first r)) (car (first r))
		(* 100 (/ (second r) error-count)))))
    (/ total-correct total-words)))

(defun run-tests ()
  (let ((accuracy nil))
    (dotimes (i 10)
      (format t "Randomizing data~%")
      (trivial-shell:shell-command "cd data && ./make-sets.pl all-pos.txt")
      (let ((start (get-universal-time))
            (a (progn (train) (test))))
        (push a accuracy)
        (format t "Time: ~F minutes~%" (/ (- (get-universal-time) start) 60))
        (format t "Accuracy: ~F~%" a)))
    (format t "Average accuracy: ~F~%" (/ (reduce #'+ accuracy) 10))))

(defun tag-ad-questions (&optional (file "data/questions.txt"))
  (with-open-file (out "data/questions-tagged.txt"
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (with-open-file (in file)
      (do ((line (read-line in nil :eof) (read-line in nil :eof)))
          ((eql line :eof))
        (dolist (triple (tag line))
          (destructuring-bind (tags words sentence) triple
            (declare (ignore sentence))
            (map nil (lambda (tag word)
                       (format out "~A/~A " word tag))
                 tags words)))
        (format out "~%")))))

(defun make-ad-lexicon (&optional (file "data/questions.txt"))
  (with-open-file (out "data/ad-lexicon.txt"
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (with-open-file (in file)
      (do ((line (read-line in nil :eof) (read-line in nil :eof)))
          ((eql line :eof))
        (dolist (word (mapcan 'make-word-list (split-sentences line)))
          (let ((pos-list (lookup-pos word)))
            (unless pos-list
              (format out "~A\\~%" word))))))))
