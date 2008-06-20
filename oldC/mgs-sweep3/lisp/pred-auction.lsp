;; (load "models")

(require "experts")
(require "auctions")
(require "pipes")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;               Models
;;
;;  Models interface operators to the underlying C code 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun MAKE-MODEL (name n p)
  (send model-proto :new name (concatenate 'string name ".db") n p)
  )

(defproto MODEL-PROTO 
  '(prefix ; file name prefix
    dataFile
    p      ; number of predictors
    n      ; number of obs
    index  ; position in current search
    preds  ; current list of operators in the model
    ))

(defmeth model-proto :PRINT ( &optional (stream t))
  (call-next-method)
  (format stream "~%Prefix           : ~a [~d,~d]~%" 
	  (slot-value 'prefix) (slot-value 'n) (slot-value 'p))
  (format stream "Current index    : ~d~%" (slot-value 'index))
  (format stream "Predictors [q=~2d]:~%" (send self :q))
  (dolist (pred (slot-value 'preds))
	  (format stream "~a~%" pred))
  t)

(defmeth model-proto :ISNEW (prefix dataFileName n p)
  (setf (slot-value 'prefix) prefix)
  (setf (slot-value 'dataFile) dataFileName)
  (setf (slot-value 'p)      p)
  (setf (slot-value 'n)      n)
  (setf (slot-value 'index)  0)
  (setf (slot-value 'preds)  ())
  (when dataFileName
    (send self :initialize-external-files))
  )

(defmeth model-proto :INITIALIZE-EXTERNAL-FILES ()
  ;; Builds the model.0 files and then exits
  (start-external-process "driver") 
  (run-external-command 
   (format nil "init ~a ~a" 
	   (slot-value 'dataFile)
	   (slot-value 'prefix)))
  (run-external-command "end 0.0")
  (end-external-process)
  t
  )

(defmeth model-proto :P ()
  (slot-value 'p))

(defmeth model-proto :N ()
  (slot-value 'n))

(defmeth model-proto :Q ()
  (length (send self :predictors)))

(defmeth model-proto :PREDICTORS ()
  (slot-value 'preds))

(defmeth model-proto :PREPARE-TO-EVAL-OPERATORS (&optional q)
  ;; first check to see if we have q or more already
  (let ((useQ (if q q (send self :q)))  )
    (when (< useQ (send self :q))
      (format t "Note: model of size ~d already exists.~%" (send self :q))
      (format t "Trimming these operators:~%")
      (dotimes (j (- (send self :q) useQ))
	       (format t "~a~%" (pop (slot-value 'preds)))  ))
    (start-external-process "driver") 
    (run-external-command
     (concatenate 
      'string "run " (slot-value 'prefix) ".gsModel." 
      (format nil "~d" useQ)))
    ))

(defmeth model-proto :EVAL-OPERATOR (op)
  (setf (slot-value 'index) (1+ (slot-value 'index)))
  (let ((result (run-external-command
		 (print-operator-to-stream
		  op nil 
		  :fToEnter (send self :f-from-index (slot-value 'index))
		  )))  )
    (when (listp result) ; accepted
      (setf (first result) (slot-value 'index)) ; search index
      (setf (slot-value 'index) 0)
      (push result (slot-value 'preds))
      )))

(defmeth model-proto :F-FROM-INDEX (index)
  (let ((indexLen  (log (/ (+ (* .5 (geometric-pdf index))
			      (* .5 (cauchy-pdf index))  ))
			2))
	(ln2        0.693147)  )
    (if (<= indexLen 5) ; use quadratic approximation 
	(^ (* 0.272727 
	      (+ 4.0 
		 (sqrt (+ 28.8374 
			  (* 22.0 ln2 indexLen) ; log 2
			  ))))
	   2)
      (* 2.0 ln2 (+ 4.0 (log ln2 2) (log indexLen 2) indexLen))
      )))	     
