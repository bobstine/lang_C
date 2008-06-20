;;
;;   Lisp file for drawing the loopies
;;
;;   17 Apr 00
;;

(defun shell ()
  
  () ; riss const = 2.865 seems big (next is only 2.3) ???
  (sum (^ 2 (- (log* (iseq 1 2000)))))


  (def p (plot-points (iseq 10) (iseq 10)))
  (add-loopie-to-plot 2 2 p)
  
  (let* ((p  20)
	 (k  1 )
	 (y (loopie-base k p))
	 (x (loopie-z  y))      )
    (format t "z's code these values: ~% ~{~5,2f ~}~%" x)
    (format t "       at base values: ~% ~{~5,2f ~}~%" y)
    (setf thePlot (plot-points x y))
    (mapcar #'(lambda (x y) (add-loopie-to-plot x y thePlot))
	    x y))
  
)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ADD-LOOPIE-TO-PLOT (x y plot)
  (let* ((k (* 2 (log 2)))
	 (range (send plot :range 0))  
	 (xax   (rseq (first range) (second range) 100))  )
    (send plot :add-lines xax
	  (+ y (mapcar #'(lambda (x) (/ (* x x) k))
		       (- xax x))))
    t))

(defun LOOPIE-BASE (probZero p)
  "Returns base height for (p+1) loops, as log p(0), log p(1)...
   so that code is viewed as probability model given input prob of zero"
  (let ((pNotZero  (- 1 probZero))  )
    (cons (- (log2 probZero))
	  (+ (- (log2 pNotZero))
	     1  ; sign bit
	     (mapcar #'monotone-universal-length (iseq 1 p))
	     ))))

(defun LOOPIE-Z (loopBaseList)
  "Returns z-score coding for sequence of loops with indicated
   base heights"
  (cons 0 (accumulate 
	   #'+
	   (mapcar #'(lambda (x)
		       (if (< x 0) .5
			 (if (< x 1) 1
			   (sqrt x))))
		   (* (* 2 (log 2))
		      (difference loopBaseList)
		      )))))

       
(defun CAUCHY-BIT-LENGTH (posInt)
  "Length for cauchy code of a postive integer."
  (* 2 (floor (1+ (log2 posInt)))))

(defun MONOTONE-UNIVERSAL-LENGTH (posInt)
  "Fractional length for arithmetic version of univ code for pos int."
  (if (and (integerp posInt) (< 0 posInt))
      (+ (log2 2.865)    ; log of normalizing const, log* 1 = 0 incld
	 (log* posInt))
    (format t "~a is not a positive integer" posInt)))


(defun LOG* (x)
  "Sum of binary recursive logs."
  (if (listp x)
      (mapcar #'log* x)
    (do* ((arg x (log2 arg))
	  (sum 0 (+ sum arg))  )
	 ((> 1 arg) sum)
	 )))
       
(defun LOG2 (x)
  (log x 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;  EOF  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
