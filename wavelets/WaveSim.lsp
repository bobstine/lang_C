;;;;  Wavelet thresholding simulations
#|
   7 Jan 97 ... Created for Biometrika draft.
|#


(require "WaveThresh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  ;;
;;   Wave-Thresh Parameters         ;;
;;                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ;--- Define parameters
   (def waveCoefs *d4*)
   (def per?      t)                    ; Use periodic coefs?
   ; noise variance SIGMA = 1 throughout


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  ;;
;;   Wave-Thresh Examples  : BB     ;;
;;                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| (set-working-directory "Dave:XLisp:Ras Lisp:Wavelets:Simulation Results")
   
   ; --- Read prior simulation results
   (load "varsim")
   (setf max 50)
   (def simres varsim)


   ; --- Generate new simulation    (randomize random state)
   (make-random-state t)
   (run-sim 1000 :limit 50)
   (def varsim50 simres)  
   ; (savevar 'varsim "varsim")
   ; (setf varsim (append simres varsim))


   ; --- Select data for display at a sample of points
   (setf snr (mapcar #'first simres))   ; true snr
   ; (setf snr (mapcar #'(lambda (x) (first (second x))) simres))   ; obs snr
   (setf opt (mapcar #'(lambda (x) (third (second x))) simres))
   (setf sin (sample (iseq (length simres)) (min 200 (length simres))))
 

   ; --- Plot MSE of methods
   (setf max 10)
   (setf func #'mse-of) (setf label "MSE")
   (setf pr (plot-method func 'aic 'black 'disc :label label :max max :addData nil))
   (plot-method func 'bic  'blue  'cross  :plot pr :max max)
   (plot-method func 'ric  'green 'x      :plot pr :max max)
   (plot-method func 'ebic 'red   'square :plot pr :max max)
   (send pr :abline 1 0)
   (send pr :x-axis t t 6) (send pr :range 0 0 max) 
   (send pr :y-axis t t 6) (send pr :range 1 0 1.25)



   ; --- Plot Relative MSE of methods
   (setf max 50)
   (setf func #'relative-mse-of) (setf label "Relative MSE")
   (setf pr (plot-method func 'aic 'black 'disc :label label :max max :addData nil))
   (plot-method func 'bic  'blue  'cross  :plot pr :max max)
   (plot-method func 'ric  'green 'x      :plot pr :max max)
   (plot-method func 'ebic 'red   'square :plot pr :max max)
   (plot-method func 'data 'cyan  'dot4   :plot pr :max max)
   (send pr :abline 1 0)
   (send pr :x-axis t t 6) (send pr :range 0 0 max) 
   (send pr :y-axis t t 5) (send pr :range 1 0 4)



   ; --- Regression slopes
   (def r (regression-model (sqrt snr) (mse-of 'bic)))
   (send r :plot-residuals)

   
   ; --- Plots of the number of nonzero coefficients
   (def p1 (plot-points (sqrt (select snr sin)) (/ (select (coefs-of 'aic ) sin) 1024)
                        :variable-labels (list "Sqrt SNR" "Proportion Nonzero")))
   (send p1 :add-points (sqrt (select snr sin)) (/ (select (coefs-of 'ebic) sin) 1024)
         :color 'red :symbol 'square)
   (send p1 :adjust-to-data)
   (send p1 :size 450 280)
   (send p1 :y-axis t t 3) (send p1 :x-axis t t 6) (send p1 :range 0 0 max)

   (def p2 (plot-points (select (coefs-of 'aic) sin) (select (coefs-of 'ebic) sin)))
   (send p2 :abline 0 1)

   (histogram (select (coefs-of 'ebic) sin))
   (plot-function #'(lambda (p) (* 1024 (- (h (+ p (/ 1024))) (h p))) )
                  .33 .334)


   ; --- bits versus MSE
   (def p (plot-points (bits-of 'aic) (mse-of 'aic)
                       :variable-labels '("Bits" "MSE")))
   (send p :add-points (bits-of 'bic) (mse-of 'bic)  :color 'blue :symbol 'cross)
   (send p :add-points (bits-of 'ric) (mse-of 'ric)  :color 'green :symbol 'x)
   (send p :add-points (bits-of 'ebic) (mse-of 'ebic) :color 'red :symbol 'square)
   (send p :adjust-to-data)




   ; --- Run simulations at fixed SNR  (note prior plot is on sqrt scale)
   (def b (boxplot (mapcar #'relative-mse-of '(aic bic ric ebic))))
   (send b :y-axis t t 4)
   (send b :variable-label 1 "Relative MSE")
   (send b :abline 1 0)

   ; --- Labelled boxplot with mands
   (load "simres40") (def simres simres40)
   (def b (compare (mapcar #'relative-mse-of '(aic bic ric ebic)) 
                   :labelOffset -.5 :yLabels 5))

   ; --- Reset random seed
   (make-random-state t)



   ; --- 0 ebic and ric are === 0
   (run-fixed-sim 500  0)      (setf simres0 simres) (savevar 'simres0 "simres0")

   ; --- 0 ebic and ric are === 0
   (run-fixed-sim 500 (^ .25 2)) (setf simres025 simres)
                                         (savevar 'simres025 "simres025")

   ; --- 1 ebic again wins; very similar to the 0.25 case though BIC is closer
   (run-fixed-sim 500  1)      (setf simres1 simres) (savevar 'simres1 "simres1")

   ; --- 2 ebic wins, with bic and ric close
   (run-fixed-sim 500 (^ 2 2)) (setf simres2 simres)    (savevar 'simres2 "simres2")

   ; --- 4 ric gone, bic is  second to ebic with ric rising to just below aic
   (run-fixed-sim 500 (^ 4 2)) (setf simres4 simres)    (savevar 'simres4 "simres4")


   ; --- 10 
   (run-fixed-sim 500 (^ 10 2)) (setf simres10 simres)    (savevar 'simres10 "simres10")

   ; --- 40 
   (run-fixed-sim 500 (^ 40 2)) (setf simres40 simres)    (savevar 'simres40 "simres40")
 
    ; (time (run-fixed-sim 5 5))  lisp 93.03 seconds; 13.35 seconds in gc
    ;                         C coders 45.05 seconds; 8.45 seconds in gc.
|#

(defun RUN-SIM (n &key (limit 50))
  (def simres ())
  (dotimes (i n)
           (format t "Iteration #~d/~d~%" (1+ i) n)
           (let ((snr (^ (* limit (first (uniform-rand 1))) 2))  )
             (push
              (cons snr 
                    (round (sim-bb snr :plot? nil :se 1 :regen? t))  )
              simres)
             )))  

(defun RUN-FIXED-SIM (n snr)
  (def simres ())
  (dotimes (i n)
           (format t "Iteration #~d/~d for snr ~5,2f~%" (1+ i) n snr)
           (push
            (cons snr 
                  (round (sim-bb snr :plot? nil :se 1 :regen? t))  )
            simres)
           ))
         
       
  ; --- Select sample spacing and signal to noise ratio
  ;     noting that Int(BB^2) = \int t(1-t) = 1/6 on [0,1]
  ;     so that for SNR to be 1, need to scale up BB by sqrt(6)
  ; --- Generate Brownian bridge on dense index
  ;    (setf z (sim-bb (* 40 40) :plot? nil :se 1 :regen? t))

(defun SIM-BB (snr &key (plot? nil) (se 2) (regen? t) )
  (let ((z  nil)                  ; Z holds result of function
        (n  4096)   )             ; Maximum resolution simulated
    (when regen?
          (def BB (tie-down
                   (cons 0 (accumulate
                            #'+ (* (sqrt (/ n)) (normal-rand (1- n)))  ))))
          (def NOISE  (normal-rand n))   )
    (def SIGNAL   (* (sqrt (* snr 6)) bb))
    (def WDSIGNAL (c-fwt signal waveCoefs :periodic? t)) 
    ; --- Generate the observable data
    (def dt     4)
    (def index  (* dt (iseq (/ n dt))))
    (def DATA   (+ (select signal index) (select noise  index)))
    (def opt    (optimal-threshold-mse data (select signal index) 0 5))
    (setf obs 
          (list (/ (sum (* signal signal)) (sum (* noise noise)))
                (sum (^ (- (select signal index) data) 2))
                (mean (second opt))  ))
    (apply #'format t "Observed SNR ~6,1f; MSE data ~6,1f, Opt ~6,1f ~%" obs)
    (format t "Optimal SS at threshold ~5,2f~%" (mean (first opt)))
    ; --- Plot data and lowess smooth superimposed on BB
    (when plot?
          (def x (rseq 0 1 (length data)))
          (def p (plot-points x data))
          (send p :size 350 350)
          (send p :title (format nil "BB: ~d obs  SNR ~d" (/ n dt) snr))
          (send p :add-lines x (select signal index) :color 'magenta)  
          ; --- Smooth with lowess
          (setf smth (lowess x data))
          (send p :add-lines  smth :color 'blue)
          (format t "MSE Lowess   ~8,2f ~%"
                  (sum (^ (- (select signal index) (second smth)) 2)))  )
    
    ; --- Add wavelet fit and compute MSE
    (dolist (method  wave-thresh-methods)
            (setf fit (wave-compress data waveCoefs method :se se))
            (when plot?
                (send p :add-lines x
                      (+ 5 (* 5 (1+ (position
                                     method (reverse wave-thresh-methods))))
                         (third fit)) :color 'green))
            (push (list (first fit) (second fit)
                        (sum (^ (- (select signal index) (third fit)) 2)))
                  z)
            (format t " Fit ~4a ~{ ~6,1f ~} ~%"  method (first z))   )
    (when plot? (send p :adjust-to-data))
    (cons obs z)
    ))


#|
   ; --- By hand analysis

    (def wd (c-fwt data waveCoefs :periodic? t))
 
   ; --- kernel density of wavelet coefficients
    (def kp (plot-lines (kernel-dens wd)))
    (send kp :add-points wd (repeat (* .05 (second (send kp :range 1))) n))
 
   ; --- Nice plot showing the effect of shadowing, entropy coding on estimator
    (def wd^ (second (ebic wd)))
    (format t "~d zero estimates.~%" (count-if #'zerop wd^))

    (def showIn (which (< (abs wd) 1)))     ; focus on data near origin
    (def cp (plot-points (select wd showIn) (select wd^ showIn)))
    (send cp :abline 0 1)

|#
            

#|
   ; --- Distribution of wavelet coefs of sampled signal

   (def kp (plot-lines 
            (kernel-dens
             (setf w 
                  (cddr
                   (c-fwt (select signal index) waveCoefs :periodic? t))))))
   (send kp :add-points w 
         (repeat (* .05 (second (send kp :range 1))) (length w)))
   (plot-points (rseq 0 1 (length w)) (sort-data (atan (* w 4))))


   ; --- Scale mixture of Gaussians does not look very Cauchy
   (let* ((u (rseq 0 1 500))
          (s (sqrt (* u (- 1 u))))  )
     (setf normMix (* s (normal-rand (length u))))
     )
   (qq-plot normMix)

   ; --- Dependence changes  Cauchy shape
   (def BB (tie-down
            (cons 0
                  (accumulate
                   #'+ (* (sqrt (/ 1024)) (normal-rand (1- 1024)))
                   ))))
   (qq-plot bb)

   ; --- its not the bb, rather its the wavelet decomp
   (def wd (fwt bb *haar* :periodic? t))
   (let ((j 10)  ) 
     (qq-plot (select wd j)) 
     (format t "sd(~d) = ~7,4f~%" j (standard-deviation (select wd j)))
     )
   (def p 
        (plot-points (iseq 4 10) 
                     (log (mapcar #'standard-deviation (select wd (iseq 4 10))) 2)
                     ))
   (send p :abline 4 -1)


   (let* ((n 100)
          (vec (rseq 0 1 n))  )
     (def cm (outer-product (- 1 vec) vec  #'*))   ; uses only upper half
     (def chol (first (chol-decomp cm)))
     (def norm (matmult chol (normal-rand n)))
     )
   
   (plot-points (rseq 0 1 (length norm)) (sort-data (atan (* norm .25))))

   (qq-plot norm)
   (print-matrix (matmult chol (transpose chol)))

|#

;;;;

(defun COMPARE (data &key (labelOffset 0) (ylabels 4))
  ; (compare (mapcar #'relative-mse-of '(aic bic ric ebic)))
  (let* (( b (boxplot data))  )
    (send b :y-axis t t ylabels)
    (send b :variable-label 1 "Relative MSE")
    (send b :abline 1 0)
    (send b :size 220 220)
    (mands data  '("AIC" "BIC" "RIC" "EBIC"))
    (defmeth b :redraw ()
      (call-next-method)
      (mapcar #'(lambda (lab x)
                  (apply #'send b :draw-string lab   
                         (send b :real-to-canvas x
                               (+ labelOffset (first  (send b :range 1))))))
              '("AIC" "BIC" "RIC" "EBIC")
              (+ (iseq 4) 0.8))  )
    b))
    
 
(defun PLOT-METHOD (extractor method color symbol &key plot max addData (label ""))
  ; Reduced lowess smoothing proportion to use 7.5% of span
  ; 13 Jan 97 ... 14 Jan 97
  (let* ((sub  (if max (which (< (sqrt snr) max)) (iseq (length snr))))
         (fit  (select (funcall extractor method) sub))
         (sm   (lowess (select (sqrt snr) sub) fit :f .075))
         (in   (* 10 (iseq (floor (/ (length sub) 10)))))
         (coor (mapcar #'rest ; result of spline is a pair of lists
                       (spline (select (first sm) in)
                               (select (second sm) in)  )))   )
    (def save coor)
    (if plot
        (send plot :add-lines   coor :color color)
        (progn
         (setf plot (plot-lines coor :color color  :symbol symbol 
                                :variable-labels (list "Sqrt SNR" label)) )
         (send plot :size 450 280)   ))
    (send plot :add-points  coor :color color :symbol symbol)
    (when addData
          (send plot :add-points (select (sqrt snr) sin) (select fit sin)
                :color color  :symbol symbol) )
    plot))



(defun BITS-OF (method)
  (let ((i (+ 2 (position method '(ebic ric bic aic))))  )
    (mapcar #'(lambda (x) (first (select x i))) simres)))

(defun COEFS-OF (method)
  (let ((i (+ 2 (position method '(ebic ric bic aic))))  )
    (mapcar #'(lambda (x) (second (select x i))) simres)))

(defun MSE-OF (method)
  (let ((n 1024)
        (sse (cond
               ((eq method 'data)
                (mapcar #'(lambda(x) (second (second x))) simres) )
               ((eq method 'opt)
                (mapcar #'(lambda(x) (third (second x))) simres)  )
               ( t
                 (let ((i (+ 2 (position method '(ebic ric bic aic))))  )
                   (mapcar #'(lambda (x) (third (select x i))) simres))
                 ))    )   )
    (/ sse n)
    ))

(defun RELATIVE-MSE-OF (method)
  (/ (mse-of method)
     (mse-of 'opt))
  )