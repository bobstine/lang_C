#| $Id: operators.lsp,v 1.1 2001/11/16 13:21:39 bob Exp $

  OperatorStreams are the "base class" for streams of recommended variables
  to consider for a regression model.
 
 15 Nov 01 ... Converted from old expert.lsp code
  1 Nov 01 ... Cut from the growing code to manage the auction

;;;;

  (load "operators")

;;-- linear stream takes two args that give range for indices
  (def  lstrm (send linear-operator-stream-proto :new 1 4))
  (send lstrm :next-operator)

;;-- quadratic stream wants array dim, then starts at (1,1)
  (def  qstrm (send quadratic-operator-stream-proto :new 4))
  (send qstrm :next-operator)

;;-- Display
  (send qstrm :status-string)

|#

(provide "operators")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;               OperatorStream
;;
;;  OperatorStreams provide a stream of operators
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto OPERATOR-STREAM-PROTO)

(defmeth operator-stream-proto :NEXT-OPERATOR ()
  (error "call to base class :next-operator"))

(defmeth auction-proto :OPERATOR-REJECTED ()
  )
(defmeth auction-proto :OPERATOR-ACCEPTED ()
  )

(defmeth operator-stream-proto :STATUS-STRING ()
  () )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Sequence experts follow some list/ordering of indices
;;
;;    Special cases include standard linear and quad orderings.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto SEQUENTIAL-OPERATOR-STREAM-PROTO
  '(
    position ; in the underlying stream; how many have been used
    )
  ()
  operator-stream-proto)

(defmeth sequential-operator-stream-proto :ISNEW ()
  (setf (slot-value 'position)
	(send self :initial-position)))

(defmeth sequential-operator-stream-proto :POSITION ()
  (slot-value 'position))
(defmeth sequential-operator-stream-proto :SET-POSITION (i)
  (setf (slot-value 'position) i))

(defmeth sequential-operator-stream-proto :NEXT-OPERATOR ()
  (send self :generate-next-operator
	(setf (slot-value 'position)
	      (send self :increment-position 
		    (slot-value 'position)))))

(defmeth sequential-operator-stream-proto :STATUS-STRING ()
  (cons (format nil "[~a]" (slot-value 'position))
	(call-next-method)))

(defmeth sequential-operator-stream-proto :INITIAL-POSITION () ;      <---- May override
  0)

(defmeth sequential-operator-stream-proto :INCREMENT-POSITION (pos) ; <---- May override
  (when pos
    (1+ pos)))

(defmeth sequential-operator-stream-proto :GENERATE-NEXT-OPERATOR (pos); <==== Override
  (error "call to base class :generate-next-operator"))

  
;;;;  Linear sequence experts

(defproto LINEAR-OPERATOR-STREAM-PROTO
  '(minPosition maxPosition)
  ()
  sequential-operator-stream-proto)

(defmeth linear-operator-stream-proto :ISNEW (min max)
  (setf (slot-value 'minPosition) min)
  (setf (slot-value 'maxPosition) max)
  (call-next-method))

(defmeth linear-operator-stream-proto :STATUS-STRING ()
  (cons "Lin " (call-next-method)))

(defmeth linear-operator-stream-proto :INITIAL-POSITION ()
  (1- (slot-value 'minPosition)))

(defmeth linear-operator-stream-proto :INCREMENT-POSITION (pos)
  (when pos
    (let ((i (1+ pos))  )
      (if (<= i (slot-value 'maxPosition))
	  i
	nil))))

(defmeth linear-operator-stream-proto :GENERATE-NEXT-OPERATOR (pos)
  (if pos
      (make-indx-operator pos)
    ()  ))


;;;;

(defproto QUADRATIC-OPERATOR-STREAM-PROTO
  '(maxIndex)
  ()
  sequential-operator-stream-proto)

(defmeth quadratic-operator-stream-proto :ISNEW (max)
  (setf (slot-value 'maxIndex) max)
  (call-next-method))

(defmeth quadratic-operator-stream-proto :STATUS-STRING ()
  (cons "Quad" (call-next-method)))

(defmeth quadratic-operator-stream-proto :INITIAL-POSITION ()
  (list 1 0)  )
    
(defmeth quadratic-operator-stream-proto :INCREMENT-POSITION (index)
  ;; Quad operator-streams use one-based index
  (when index
    (let ((i1 (first index))
	  (i2 (second index))  )
      (if (< (1+ i1) i2)
	  (list (1+ i1) (1- i2))
	;; next transverse diagonal, watching for corners
	(let ((sum (+ i1 i2))
	      (p   (slot-value 'maxIndex))  )
	  (if (<= sum p)
	      (list 1 sum) ; back to first row
	    (if (= sum (* 2 p))         ; end of the road
		()
	      (list (- (+ 1 sum) p) p)  ; lower right corner requires move to last column
	     )))))))
  
(defmeth quadratic-operator-stream-proto :GENERATE-NEXT-OPERATOR (pos)
  (if pos
      (make-quad-operator pos)
    ()  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;               Operators
;;
;;  Operators are just lists, whose head matches the form of the 
;;  4 character key used in the C program.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
  (def ops (mapcar #'(lambda (i) (make-operator 'indx i))
                   (iseq 1 10)))
  (mapcar #'(lambda (op) (print-operator-to-stream op t))
          ops)
|#

(def operatorTypes '(INDX LINC INTR))

(defun MAKE-OPERATOR (type &rest args)
  (case type
	('indx (apply #'make-indx-operator args))
	('intr (make-intr-operator args))
	('linc (make-linc-operator args))
	(  t   (format t "Operator of type ~a is invalid.~%" type))
	))

(defun MAKE-INDX-OPERATOR (index)
  (list 'indx (list index)))

(defun MAKE-QUAD-OPERATOR (index)
  ;; Interaction needs to indicate its order (thus the cons of 2 on list)
  (list 'intr (cons 2 (select index '(0 1)))))

(defun PRINT-OPERATOR-TO-STREAM (operator stream &key (fToEnter " "))
  (format stream "~a ~8,3f ~{ ~a ~}"
	  (first operator) fToEnter (second operator)))

(defun READ-OPERATOR-FROM-STREAM (stream)
  (let ((list (with-input-from-string (s (read-line stream))
				      (read s)))  )
    (make-operator (first list) (rest list))
    ))

