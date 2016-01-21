NLP for common Lisp by Kevin Raison

NLP utilities for Common Lisp, including part of speech tagger, phrase chunker,
pure Lisp wordnet interface and more.

First, load the library:

  ```
  (ql:quickload :nlp)
  (use-package :nlp)
  ```

Then, download or train an NLP data file:

  ```wget http://chatsubo.net/~raison/nlp.dat```

  ```
  (thaw-nlp "nlp.dat")
  ```
or

  ```
  (build-nlp :pos-train "data/all-pos.txt"
             :grammar-load "data/p-grammar.txt"
             :chunker-train "data/all-parsed.txt"
             :pos-lex "data/all-pos.txt"
             :moby-file "data/moby/mobypos.txt"
             :contraction-file "data/contractions.txt"
             :user-lexicon-file data/user-lexicon.txt)
  (populate-wordnet-database :path "prolog/")
  ;; Save the training data file
  (freeze-nlp "new-nlp.dat")
  ```
  
Now you can tag sentences:

  ```
  (tag "This is a test. This is only a test")
  ```

Or chunk them:

  ```
  (all-phrases "This is a test. This is only a test.")
  ```

Or query wordnet:

  ```
  (synonyms "hand")
  (hypernyms "hand")
  (hyponyms "hand")
  ```

There are many more features;  documentation is forthcoming!
