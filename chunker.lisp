(in-package #:nlp)

(defun chunk (text)
  "Unfinished chunker"
  (let ((layers nil) (layer0 nil) (leftover nil))
    (loop
       for s being the hash-keys in (pos-cfg *pos-db*)
       using (hash-value p)
       do (if (hash-table-p p)
	      (push s layer0)
	      (push s leftover)))
    (labels ((build-layers (leftover)
	       leftover))
      (build-layers leftover))
    (push (sort layer0 'string< :key 'symbol-name) layers)
    (build-layers leftover)
    (nreverse layers)))
