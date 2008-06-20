;;;;;;;;;;;;;;  testing gsl

(def   y '(0   0   0   1   0   1   1   0   1   1))
(def  x1 '(0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0))
(def  x2 '(0.1 1.1 2.2 3.2 4.1 5.3 6.2 7.1 8.4 9.5))

(format t "Means : ~a: ~a  ~a ~%"	(mean y) (mean x1) (mean x2))

(def regr (regression-model (list x1) y :predictor-names '("X1")))
(format t "Resids Y on X1: ~{ ~6,3f ~}~%" (send regr :raw-residuals))
(def fit  (pmax 0.000001 (pmin 0.999999 (send regr :fit-values))))

(def regr (regression-model (list x1) x2 :predictor-names '("X1")))
(format t "Resids X2 on X1: ~{ ~6,3f ~}~%" (setf z (send regr :raw-residuals)))
(format t "RSS_z = ~7,4f~%" (setf rssz (inner-product z z)))
(format t "beta_z = ~7,4f~%" (/ (inner-product z y) rssz))
(format t "max A = ~7,4f~%" (max (* (abs z) (pmax fit (- 1 fit)))))
(format t "sum b^2 = ~7,4f~%" (sum (* z z fit (- 1 fit))))

(def regr (regression-model (list x1 x2) y :predictor-names '("X1" "X2")))
(format t "Resids Y on X1 & X2: ~{ ~6,3f ~}~%" (send regr :raw-residuals))

;;;;;;;;;;;;;;  testing model
(def data (read-data-columns "/Users/bob/C/regression/test/model"))

(def wts  (select data 0))
(def   y  (select data (1+ 0)))
(def  x1  (select data (1+ 1)))
(def  x4  (select data (1+ 4)))
(def  x4s (* x4 x4))

(format t "Means : ~a: ~a ~a ~a ~%"	(mean y) (mean x1) (mean x2) (mean xx))

(format t "Cross-products: ~%")
(def yxMat (bind-columns (- y (mean y)) (- x1 (mean x1)) (- x2 (mean x2)) (- xx (mean xx))))
(print-matrix (cross-product yxMat) t :float-digits 4)

(def regr (regression-model (list x1 x4 x4s) y))
(format t "1 df F-stats for each predictor ~{ ~7,4f~}~%" 
	(setf fstat
	      (^ (/ (send regr :coef-estimates)
		    (send regr :coef-standard-errors))
		 2)))
(format t "p vals for F tests ~{ ~7,4f~}~%"
	(- 1 (f-cdf 1 (- (length y) 4) fstat)))
