;;;;;;;;;;;;;;  testing covariances and sweep matrix

(load "/Users/bob/raslisp/utilities/utils.lsp")

(defun read-col (f)
  (with-input-from-string (s (concatenate 'string "(" (read-line f) ")"))
			  (read s)))

(defun pr (x)
  (format t "x = ~a~%" x) x)

(with-open-file (f "/Users/bob/C/seq_regr/test/covar.dat")
		(let (( n (pr (read f)))
		      ( p (pr (read f)))  )
		  (def y (read-col f))
		  (def x ())
		  (dotimes (j (1- p))
		    (push (read-col f) x))
		  (setf x (reverse x))
		  nil))

(format t "Means : ~6,3f, ~a ~%"	(mean y) (mapcar #'mean x))

(print-matrix 
 (* (1- (length y)) (apply #'covariance-matrix y x)) t :float-digits 3)

;; label the predictors

(def x1 (select x 0))
(def x2 (select x 1))
(def x3 (select x 2))
(def x4 (select x 3))

;; find position of max corr (x4) and regress on this one

(mapcar #'(lambda (z) (corr y z)) (list x1 x2 x3 x4))

(def r (regression-model x4 y))
(send r :residual-sum-of-squares)

(mapcar #'(lambda (j) 
	    (def r (regression-model (select x (list 3 j)) y :print nil))
	    (format t "j= ~d with rss = ~7,3f~%" j (send r :residual-sum-of-squares)))
	'(0 1 2))

(def r (regression-model (list x4 x3) y))
(send r :residual-sum-of-squares)

(def r (regression-model (list x4 x3 x1) y))
(send r :residual-sum-of-squares)


;; --- Sweep Matrix

(def sm (make-sweep-matrix (bind-columns x1 x2 x3 x4) y))
(print-matrix sm t :float-digits 3)
(print-matrix (first (sweep-operator sm '(4))) t :float-digits 4)
