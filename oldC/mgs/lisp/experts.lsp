#|
 
  1 Nov 01 ... Cut from the growing code to manage the auction

  Experts have become the "base class" for streams of recommended variables
  to consider for a regression model.

;;;;

  (load "experts")

;;-- Make a shell
  (def model (send model-proto :new nil nil 100 10))

;;-- Experts for this model, with initial balance of 10
  (def qe (send quadratic-expert-proto :new model 10))
  (format t "~6,2f ~a~%" (send qe :probability) (send qe :operator))
  (send qe :next-operator)
  (send qe :bid)

;;-- Display
  (send qe :display)

  (def w (send expert-window-proto :new :size '(800 100)))
  (send w :add-expert qe)

|#

(provide "experts")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
#| $Id: experts.lsp,v 1.1 2002/05/31 21:40:09 bob Exp $
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               Expert
;;
;;  Experts are objects that provide a stream of operators
;;  with grades (probabilities) attached indicating preferences.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto EXPERT-PROTO 
  '(balance      ; accumulated value
    model        ; model object making the suggestions for
    currentOp    ; most recent operator, as (prob op)
    history      ; list of prior considered bids, as (result$ prob op)
    ))

(defmeth expert-proto :ISNEW (model initialBalance)
  (setf (slot-value 'balance)   initialBalance)
  (setf (slot-value 'model)     model)
  (setf (slot-value 'currentOp) (send self :next-operator))
  )

;;;;   accessors

(defmeth expert-proto :BALANCE ()
  (slot-value :balance))

(defmeth expert-proto :SET-BALANCE (balance)
  (format t "Resetting expert balance from ~a to ~a~%"
	  (slot-value 'balance) balance)
  (setf (slot-value 'balance) balance))

(defmeth expert-proto :MODEL ()
  (slot-value 'model))

(defmeth auction-proto :HISTORY ()
  (slot-value 'history))

;;;;   bidding

(defmeth expert-proto :BID ()
  ;; Next bid for operator given state of model and expert
  (unless (slot-value 'currentOp)
    (setf (slot-value 'currentOp) (send self :next-operator)))
  (let ((availBal (* .5 (slot-value 'balance)))  )
    (if (< 0 availBal)
	(let ((p (send self :probability)) ); prob of winning $1
	  (if (= p 1)
	      availBal
	    (min availBal (/ p (- 1 p)))
	    ))
      0)))

(defmeth expert-proto :PROBABILITY ()
  ;; Expert's probability that current choice will work
  (first (slot-value 'currentOp)))
  
(defmeth expert-proto :OPERATOR ()
  ;; Operator for the current bid
  (second (slot-value 'currentOp))
  )

(defmeth expert-proto :save-results (price choice)
  ;; move current operator onto list of past, adjust balance
  (setf (slot-value 'balance)
	(+ (slot-value 'balance) price))
  (push (cons price choice) (slot-value 'history))
  )

(defmeth expert-proto :OPERATOR-REJECTED (price); <------ May override
  ;; Informs the expert that its choice was not used in model
  (send self :save-results (- price) (slot-value 'currentOp))
  (setf (slot-value 'currentOp) ()))

(defmeth expert-proto :OPERATOR-ACCEPTED () ;     <------ May override
  ;; Informs the expert that its choice was used in model
  (send self :save-results 1 (slot-value 'currentOp))
  (setf (slot-value 'currentOp) ()))

(defmeth expert-proto :MODEL-HAS-CHANGED () ;      <-------- May override
  ;; Informs expert that model has changed since last saw it
  )

(defmeth expert-proto :NEXT-OPERATOR ()          ; <======== Must override
  ;; Fill the currentOp slot with (prob op) list
  (format t "Call to generic method :next-operator~%")
  nil)


;;;;   display

(defmeth expert-proto :DISPLAY (&optional window x y)
  ;; Draw in window if available, or print to listener
  (if window
      (send self :draw-in-window window x y)
    (format t "~{~a ~}~%" (send self :status-string))
    ))

(defmeth expert-proto :DRAW-IN-WINDOW (w x y)
  ;; if window, then put at input x positions if given a list of them
  (let* ((str  (send self :status-string))
	 (xPos (if (and (listp x) (= (length x) (length str))) x
		 (accumulate #'+ (cons x (+ 1 (mapcar #'length (butlast str)))))
		 ))  )
    (format t "~a~%" xpos)
    (mapcar #'(lambda (s x) (send w :draw-string s x y))
	    str (* 10 xPos))
    ))

(defmeth expert-proto :STATUS-STRING ()              ; <-------- May override
  (mapcar
   #'(lambda (s x) (format nil s x))
   (list "p=~6,4f" "~a"  "Bal=~,2f" "#hist=~d")
   (list  (first (slot-value 'currentOp))
	  (second (slot-value 'currentOp))
	  (slot-value 'balance) 
	  (length (slot-value 'history)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto EXPERT-WINDOW-PROTO
  '(experts)
  ()
  graph-window-proto)

(defmeth expert-window-proto :ADD-EXPERT (expert)
  (push expert (slot-value 'experts))
  (send self :redraw)
  )

(defmeth expert-window-proto :REDRAW ()
  (call-next-method)
  (let ((x 1)
	(y 0) (dy 10)  )
    (dolist (e (slot-value 'experts))
	    (send e :display self x (incf y dy))  )
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Sequence experts follow some list/ordering of indices
;;
;;    Special cases include standard linear and quad orderings.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto SEQUENCE-EXPERT-PROTO
  '(geoRate
    geoShare
    nSinceLastHit
    index ; denotes 'position' in the underlying sequence, 1 based
    )
  ()
  expert-proto)

(defmeth sequence-expert-proto :ISNEW (model balance geoRate geoShare)
  (setf (slot-value 'geoRate)       geoRate)
  (setf (slot-value 'geoShare)      geoShare)
  (setf (slot-value 'index)         (send self :initial-index))
  (setf (slot-value 'nSinceLastHit) 0)
  (call-next-method model balance) ;call last since will init operator
  )

(defmeth sequence-expert-proto :STATUS-STRING ()
  (cons
   (format nil "i=~3d gRate=~4,2f gShr=~4,2f"
	   (slot-value 'index)
	   (slot-value 'geoRate) (slot-value 'geoShare))
   (call-next-method)))

(defmeth sequence-expert-proto :CODING-PROBABILITY ()
  (let ((gs (slot-value 'geoShare))
	(i  (1+ (slot-value 'nSinceLastHit)))  )
    (+ (*    gs    (geometric-pdf i :rate (slot-value 'geoRate)))
       (* (- 1 gs) (cauchy-pdf i))
       )))
			
(defmeth sequence-expert-proto :NEXT-OPERATOR ()
  (setf (slot-value 'nSinceLastHit)
	(1+ (slot-value 'nSinceLastHit)))
  (setf (slot-value 'index)
	(send self :increment-index (slot-value 'index)))
  (setf (slot-value 'currentOp)
	(list (send self :coding-probability)
	      (send self :operator-for-index (slot-value 'index))
	      )))

(defmeth sequence-expert-proto :OPERATOR-ACCEPTED () ;  <---- May override
  (setf (slot-value 'nSinceLastHit) 0)
  (call-next-method))

(defmeth sequence-expert-proto :INITIAL-INDEX () ;      <---- May override
  0)

(defmeth sequence-expert-proto :INCREMENT-INDEX (index) ; <-- May override
  (1+ index))
  
(defmeth sequence-expert-proto :OPERATOR-FOR-INDEX (index) ; <== Must override
  (format t "Error: call to generic :sequence-operator~%")
  nil)

  
;;;;  Linear sequence experts

(defproto LINEAR-EXPERT-PROTO
  ()
  ()
  sequence-expert-proto)

(defmeth linear-expert-proto :ISNEW (model balance &key (geoRate .5) (geoShare .6))
  (call-next-method model balance geoRate geoShare))

(defmeth linear-expert-proto :STATUS-STRING ()
  (cons "Lin " (call-next-method)))

(defmeth linear-expert-proto :INCREMENT-INDEX (index)
  (let ((i (1+ index)))  
    (if (<= 1 i (send (slot-value 'model) :p))
	i
      1)))

(defmeth linear-expert-proto :OPERATOR-FOR-INDEX (index)
  (make-indx-operator index))


;;;;

(defproto QUADRATIC-EXPERT-PROTO
  '()
  ()
  sequence-expert-proto)

(defmeth quadratic-expert-proto :ISNEW (model balance &key (geoRate .3) (geoShare .3))
  (call-next-method model balance geoRate geoShare))

(defmeth quadratic-expert-proto :STATUS-STRING ()
  (cons "Quad" (call-next-method)))

(defmeth quadratic-expert-proto :INITIAL-INDEX ()
  (list 1 0)  )
    
(defmeth quadratic-expert-proto :INCREMENT-INDEX (index)
  ;; Quad experts use one-based index
  (let ((i1 (first index))
	(i2 (second index))  )
     (if (< (1+ i1) i2)
	 (list (1+ i1) (1- i2))
       ;; next transverse diagonal, watching for corners
       (let ((k (+ i1 i2))
	     (p (send (slot-value 'model) :p))  )
	 (if (<= k p)
	     (list 1 k) 
	   (if (<= k (* 2 p))
	       (list (- k p) p)
	     (list 1 1))
	   )))))

(defmeth quadratic-expert-proto :OPERATOR-FOR-INDEX (index)
  (make-quad-operator index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;               Probability utilities
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun CAUCHY-PDF (index)
  (if (< index 1)
      (format t "Index error in Cauchy~%")
    (/ 0.9287862 (+ 1 (* index index)))
    ))

(defun GEOMETRIC-PDF (index &key (rate .5))
  (if (< index 1)
      (format t "Index error in Geometric~%")
    (* rate (^ (- 1 rate) (- index 1)))
    ))


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

