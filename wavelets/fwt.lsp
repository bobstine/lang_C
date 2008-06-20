;; $Id: fwt.lsp,v 1.7 2000/04/25 21:32:43 bob Exp $-*- lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  
;;  Implements fwt with various types of thresholding
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shell ()
  (load "fwt.lsp")
  
  ;; add t argument if want spies
  (make-wavelet-engine "daub6" t)
  
  (format t "Current wavelet basis is ~a~%" *pipes-open*)

  ;; test of loop coder
  (def fit    (fwt (rseq 0 1 32) '("loop" 2)))

  ;; build some test data
  (def n      64)
  (def signal (sin (rseq 0 (* 4 pi) n)))
  (def noise  (normal-rand (length signal)))
  (def y      (+ signal noise))

  ;; can get one wavelet transform
  (def fit    (fwt y '("loop" 2)))

  ;; with a negative sign, you get the filtered coefs
  (def coefs  (fwt y '("hard" -3)))
  (def coefs  (fwt y '("loop" -1)))

  ;; or can get the raw, unfiltered coefs
  (def coefs  (fwt y '("coefs" 0)))

  ;; can get both coefs and fit
  (def fit    (fwt y '(("coefs" 0) ("loop" 1))))

  ;; can do several thresholding fits and then plot the results
  (def fit    (fwt y '(("coefs" 0) ("loop" 1) ("soft" 4) ("hard" 6))))
  (let ((x (iseq n))  )
    (def plot (plot-points x y))
    (send plot :add-lines x signal :color 'magenta)
    (dolist (f  (rest fit)) ; drop coefs
      (send plot :add-lines x (second f) :color 'green))
    )

  ;; check the ss of coefs and data
  (let ((c (second (first fit)))  )
    (format t "SS(data) = ~8,3f   SS(coefs)= ~8,3f~%"
      (inner-product y y) 
      (inner-product c c)))

  (close-wavelet-engine)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  
;;          Load the code that builds pipes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "/home/bob/xlisp/link_lisp/driver.lsp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Define the fwt functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *pipes-open* nil)

(defun MAKE-WAVELET-ENGINE (waveName &optional spy?)
  "Sets up pipes and wavelet engine using coefs of named type."
  (make-pipes "fwt" spy?)
  (when *pipes-open* (close-wavelet-engine))
  (if (stringp waveName) 
      (progn (send-to-pipe waveName)
	     (setf *pipes-open* waveName))
    (progn (format t "Name must be a string (e.g. daub6)")
	   (setf *pipes-open* nil))
    ))

(defun CLOSE-WAVELET-ENGINE ()
  "Terminates the pipes if open."
  (send-to-pipe 0)
  (setf *pipes-open* nil)
  )

(defun FWT (x threshPair)
  "fwt(data (list of pairs)) returns the wavelet decompositions
   described by the threshold (name value) pairs (such as 'hard' 2)."
  (if *pipes-open*
      (let ((threshList (if (listp (first threshPair)) threshPair
			  (list threshPair)))   )
	(send-to-pipe (length x))
	(send-list-to-pipe x)
	(butlast ; drop final response to "done"
	 (mapcar #'(lambda (pair)
		     (send-to-pipe (apply #'format nil "~a ~a" pair))
		     (unless (string= (first pair) "done") (read-from-pipe)))
		 (concatenate 'list threshList (list '("done" ""))))  ))
    (format t "Pipes are not open; fwt unable to run.~%")
    ))

;;;;;;;;;;;;;;  EOF  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





