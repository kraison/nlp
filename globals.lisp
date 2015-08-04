(in-package #:nlp)

(defvar *pos-db* nil)

(defparameter *whitespace* '(#\Space #\Newline #\Return #\Tab))

(defparameter *sentence-start* (intern "<S>" 'nlp))
(defparameter *sentence-end* (intern "</S>" 'nlp))
