(in-package #:cl-user)

(defpackage #:nlp
  (:use #:cl #:cl-ppcre #:parse-number #:cffi-wordnet #:dso-lex #:yacc)
  (:export #:*pos-db*
	   #:init-nlp
	   #:build-nlp
	   #:reset-nlp
	   #:edit-distance
	   #:split-sentences
	   #:tokenize
	   #:train-tagger
           #:extract-phrases
	   #:tag-sentence
	   #:tag
	   #:tag-as-text
	   #:earley-parse
	   #:chart-parse
	   #:cyk-parse
	   #:pcp-parse
	   #:p-chart-parse
	   #:in-lexicon?
           #:add-pos-regex
           #:add-to-lexicon
	   #:lookup-pos
	   #:possible-tags
           #:pos-similarity
           #:tree-similarity
           #:pos-edit-distance
           #:spell-check
           #:correct-spelling
           #:get-synonyms))
