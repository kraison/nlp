;; ASDF package description for nlp              -*- Lisp -*-

(defpackage :nlp-system (:use :cl :asdf))
(in-package :nlp-system)

(defsystem nlp
  :name "Natural Language Processing Utilities"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "0.9"
  :description "NLP Utils"
  :long-description "NLP Utils."
  :depends-on (:cl-ppcre
               :cl-store
               :parse-number
               :cffi-wordnet
               :dso-lex
               :yacc
               :alexandria
               :graph-utils)
  :components ((:file "nlp-package")
	       (:file "globals" :depends-on ("nlp-package"))
	       (:file "utilities" :depends-on ("globals"))
	       (:file "db" :depends-on ("utilities"))
	       (:file "corpora" :depends-on ("utilities"))
               (:file "singularize" :depends-on ("globals"))
	       (:file "lexicon" :depends-on ("utilities"))
               (:file "fuzzy" :depends-on ("utilities"))
	       (:file "sentence-splitter" :depends-on ("utilities"))
	       (:file "edit-distance" :depends-on ("utilities"))
	       (:file "transducer" :depends-on ("utilities"))
	       (:file "ngrams" :depends-on ("utilities"))
	       (:file "pos-tag" :depends-on
                      ("corpora" "lexicon" "db" "sentence-splitter"))
	       (:file "grammar" :depends-on ("corpora" "lexicon" "db"))
	       (:file "parser" :depends-on ("grammar" "pos-tag"))
	       (:file "chunker" :depends-on ("grammar" "pos-tag"))
               (:file "prob-parser" :depends-on ("parser"))
	       (:file "pcp" :depends-on ("parser"))
	       (:file "initialize" :depends-on ("prob-parser" "pcp"))))
