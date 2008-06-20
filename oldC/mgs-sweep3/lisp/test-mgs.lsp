(defun script-to-test-gs-model ()
  
  ;; --- Read the test data file
  (with-open-file (f "test/gsmodel" :direction :input)
		  (def data ())
		  (dotimes (i 10)
			   (with-input-from-string 
			    (s (concatenate 'string "( " (read-line f) " )"))
			    (push (read s) data)))
		  )
  (def data (transpose (nreverse data)))
  
  ;; --- Define variables by name
  (def wts (select data 0))
  (def y   (select data 1))
  (def x1  (select data 2))
  (def x2  (select data 3))
  (format t "Weighted means of vars are ~{ ~6,2f ~}~%"
	  (mapcar #'weighted-mean 
		  (list y x1 x2) (list wts wts wts)))
  (format t "TSS (weighted) = ~,2f~%" 
	  (inner-product wts (^ (- y (weighted-mean y wts)) 2)))

  ;; --- Fit weighted regression of y on x1, then x1 and x2
  (def r1 (regression-model x1 y :weights wts))
  (format t " squared t - ratios ~a~%"
	  (^ (/ (send r1 :coef-estimates)
		(send r1 :coef-standard-errors)) 2))
  
  (setf ssx (sum (* (- x1 (weighted-mean x1 wts))
		    (- x1 (weighted-mean x1 wts)) 
		    wts)))
  (def   z~ (/ (- x1 (weighted-mean x1 wts)) (sqrt ssx)))

  (setf beta (/ (sum (*  (- y (weighted-mean y wts))
			 z~
			 wts))
		(sqrt ssx)))

  (format t "resids ~A~%" (send r1 :raw-residuals))

  (def r2 (regression-model (list x1 x2) y :weights wts))
  (format t "resids ~A~%" (send r2 :raw-residuals))
  (format t " squared t - ratios ~a~%"
	  (^ (/ (send r2 :coef-estimates)
		(send r2 :coef-standard-errors)) 2))
  (sum (* wts (send r2 :raw-residuals) (send r2 :raw-residuals)))


)
(defun weighted-mean (x w)
  (/ (inner-product x w) (sum w))
  )

(defun pr (x)
  (format t "~% --pr--->  ~a ~% " x) x)

(def y    (second data))
(format t "Mean Y = ~7,3f~%" (mean y))
(def x2    (select data 3))
(format t "Mean X2 = ~7,3f~%" (mean x))
(format t "SS   X2 = ~7,3f~%" (sum (^ (- x2 (mean x2)) 2)))
(def x5    (select data 6))
(format t "Mean X5 = ~7,3f~%" (mean x5))
(format t "SS   X5 = ~7,3f~%" (sum (^ (- x5 (mean x5)) 2)))

(def rm (regression-model (list x2)  y))
(format t "Total SS ~8,3f~%" (send rm :total-sum-of-squares))
(format t "Resid SS ~8,3f~%" (send rm :residual-sum-of-squares))

(def rm (regression-model (list x2 x5)  y))
(format t "Total SS ~8,3f~%" (send rm :total-sum-of-squares))
(format t "Resid SS ~8,3f~%" (send rm :residual-sum-of-squares))

