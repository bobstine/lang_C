;; --- Read the test data file

(with-open-file (f "test.dat" :direction :input)
		(read-line f)
		(def data ())
		(dotimes (i 20)
			 (with-input-from-string 
			  (s (concatenate 'string "( " (read-line f) " )"))
			  (push (read s) data)))
		)
					
(def data (transpose data))
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

