(in-package #:cl-user)

(defpackage #:nlp
  (:use #:cl #:cl-ppcre #:parse-number #:cffi-wordnet #:dso-lex #:yacc #:graph-utils)
  (:export #:*pos-db*
	   #:init-nlp
	   #:build-nlp
	   #:reset-nlp
	   #:edit-distance
	   #:split-sentences
	   #:tokenize
	   #:train-tagger
	   #:tag-sentence
	   #:tag
	   #:tag-as-text
	   #:earley-parse
	   #:chart-parse
	   #:cyk-parse
	   #:pcp-parse
	   #:p-chart-parse
           #:extract-phrases
           #:all-phrases
           #:train-phrase-extractor
           #:singularize
	   #:in-lexicon?
           #:add-to-lexicon
	   #:lookup-pos
	   #:possible-tags
           #:pos-similarity
           #:tree-similarity
           #:pos-edit-distance
           #:spell-check
           #:correct-spelling
           #:singularize
           #:get-synonyms))
