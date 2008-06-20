(defun SCRIPT ()
  (load "experts")
  (load "pred-auction")
  
  ;;-- Generate a new test data set "test.db" with indicated structure
  (def p  15)
  (def n 300)
  (generate-test-data-file n p #'gen-rich-y)

  ;;-- Open pipes (only need to run this if the pipe 'files' do not exist)
  (make-pipes)

  ;;-- Make model object and initialize assciated C files, empty model
  (def testModel (make-model "test" n p))
  (send testModel :initialize-empty-model)
  (send testModel :print)

  ;;-- Try operators directly
  (send testModel :prepare-to-eval-operators 0) ; add to model 0
  (send testModel :eval-operator (make-indx-operator 1))
  (send testModel :eval-operator (make-indx-operator 2))
  (send testModel :eval-operator (make-indx-operator 3))
  (send testModel :print)
  (end-external-process) ; run this if none are added to end c process

  ;;-- Generate operators from an expert
  (def linearExpert (send linear-expert-proto :new testModel))
  (def quadExpert (send quadratic-expert-proto :new testModel))

  (def expert quadExpert)
  (setf price (send expert :bid))
  (format t "~{~a ~}~%" (send expert :status-string))
  (send expert :operator-rejected price)

  ;;-- Finally, via an auction object @
  (def linearExpert (send linear-expert-proto :new testModel))
  (def quadExpert (send quadratic-expert-proto :new testModel))
  (def auction (send auction-proto :new 
                    testModel (list linearExpert quadExpert) t)) ; show window?
  (send auction :run-auction 4)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;        Generate simulated data for tests
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun GEN-RANDOM-Y (x)
  (push (first (normal-rand 1))
	x))

(defun GEN-ONEX-Y (x)
  (let ((e (first (normal-rand 1)))  )
    (push (+ e (* (elt x 3) 10))
	  x)))

(defun GEN-RICH-Y (x)
  ;; Note that C  numbers preds one-based, not zero-based.
  (let ((e (first (normal-rand 1)))
	(p (length x))  )
    (push (+ e (+ (* 15  (elt x 0)) 
		  (*  5  (elt x 1))
		  (*  5  (elt x 2))
		  (* 20  (* (elt x 0) (elt x 2))))  )
	  x)))

(defun GENERATE-TEST-DATA-FILE (n p func)
  (let ((data (mapcar func ; push y_i onto front of each x_i
		       (split-list (normal-rand (* n p)) p))  ))
    (with-open-file (f "test.db" :direction :output)
		    (format f "~d ~d~%" n (+ 2 p)) ; room for wts, y
		    (dolist (obs data)
			    (format f "1.0 ~{ ~6,3f ~}~%" obs)) ; prefix with weight
		    )
    (format t "Done; wrote ~d obs to file test.db~%" n)
    ))


