;;;;  Wavelet thresholding simulations
#|

   ; Load this file to define functions, then use scripts below
   (load "/home/bob/C/wavelets/wave-sim.lsp")

|#

  (load "/home/bob/C/wavelets/fwt.lsp")
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                
;;  Make initial wavelet engine
;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (make-wavelet-engine "daub6" t)  ;; add t argument if want spies
  (format t "Current wavelet basis is ~a~%" *pipes-open*)

#|
  ; close when done
  (close-wavelet-engine)
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Define the data generator
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
  (def n 128)
  (def x (generate-signal n :amplitude 0))
  (def p (plot-lines (iseq n) x :color 'magenta))
  (send p :add-points (iseq n) (+ x (generate-noise n)))
  (send p :adjust-to-data)
|#

(defun GENERATE-SIGNAL (n &key (amplitude 20) (cycles 15))
  (* (/ 2 (1+ (exp (rseq 0 5 n))))
     (* amplitude (sin (rseq 0 (* cycles 2 pi) n)))  ))

(defun GENERATE-NOISE (n)
  (normal-rand n))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Compute MSE of wavelet estimator(s)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
  (def n     512)
  (def nReps  10)
  (def amp     5)
  (def mse  (wavelet-mse n nReps amp
                  (list  (list "hard"  0.0)
                         (list "hard"  (sqrt (* 2 (log n))))
                         (list "soft"  (sqrt (* 2 (log n))))
                         (list "loop"  1)  )))
  (boxplot (transpose mse))

  (scatterplot-matrix (transpose mse)) ; busted?

  (def p (plot-points (second (transpose mse)) (fourth (transpose mse))))
  (send p :abline 0 1)

|#

(defun WAVELET-MSE (nLen nReps amplitude wavePairs)
  (let ((result ())  )
    (dotimes (i nReps)
	     (format t "i = ~a~%" i)
	     (let ((signal (generate-signal nLen :amplitude amplitude))  )
	       (push 
		(mapcar #'(lambda (z) (mse signal (second z)))
			(fwt (+ signal (generate-noise nLen))
			     wavePairs))
		result)))
    result))
		       
(defun MSE (truth fit)
  (let ((d (- fit truth)) )
    (/ (inner-product d d) 
       (length d)) ))
		      
(defun DPR (msg x)
  (format t "~a ~a~%" msg x)
  x)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  EOF  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
