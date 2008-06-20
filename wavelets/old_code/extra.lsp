   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;    Wavelet signal with chosen distribution of coefs
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
   ;--- Spike
   (def wd  (combine (repeat 0 16) '(4 4) (repeat 0 494)))


   ;--- Randomized coefficients from some dist
   (def wd (* (select '(-1 1) (binomial-rand n 1 .5))
               (combine 
                (reverse (sort-data (abs (cauchy-rand (/ n 2)))))
                (repeat 0 (/ n 2))  )))
   (def WAVE (c-ifwt wd wavecoefs :periodic? per?))


   ;--- Fixed size coefficients
   (def wd (combine (repeat 3 (/ n 2)) (repeat 0 (/ n 2))))
   (def WAVE (c-ifwt wd wavecoefs :periodic? per?))


   ;--- plot illustrative example
   (def p  (plot-lines time wave))

   ;--- plot wavelet coef
   (plot-wd wd)

|#



   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;   Sinusoidal example (principal + harmonics + random phase)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
   ; --- simple sine 
   (def  SNR  8)
   (setf harmonics '(1))              ; weights for harmonic amplitudes
   (setf amplitude (sqrt (/ (* 2 snr) (sum (^ harmonics 2)))))
   (def  freq  (/ (* '(1)   2 pi) n)) ; fundamental freq
   (def  phase (* 2 pi (uniform-rand (length harmonics))))
   (def  SINE (* amplitude 
                (apply #'+
                       (mapcar #'(lambda (a f p) 
                                   (* a (sin (+ p (* f time)))))
                               harmonics freq phase))))

   ; --- complex mixture of sines
   (def  SNR  8)
   (setf harmonics '(1 .5 .5 .25))          ; weights for harmonic amplitudes
   (setf amplitude (sqrt (/ (* 2 snr) (sum (^ harmonics 2)))))
   (def  freq  (/ (* '(1 2 4 8)   2 pi) n)) ; fundamental freq
   (def  phase (* 2 pi (uniform-rand (length harmonics))))
   (def  SINE (* amplitude 
                (apply #'+
                       (mapcar #'(lambda (a f p) 
                                   (* a (sin (+ p (* f time)))))
                               harmonics freq phase))))

   ;--- plot illustrative example
   (setf data (+ sine (normal-rand n)))
   (def p  (plot-lines time sine))
   (send p :add-points time data)
   (format t "obs SNR = ~6,2f~%"  
           (/ (sum (* sine sine))  (sum (^ (- data sine) 2))))

  ;--- plot it

   (def p (plot-lines time sine :variable-labels '("Time" "Sinusoid")))
   (send p :size 400 300)
   (send p :x-axis t t 3)
   (send p :range 1 -5 5)
   (send p :range 0 1 512)

   (def p2 (plot-points time data :variable-labels '("Time" "Sine + Noise")
                        :symbol 'dot4))
   (send p2 :size 400 300)
   (send p2 :range 1 -5 5)
   (send p2 :y-axis t t 5)
   (send p2 :x-axis t t 3)
   (send p2 :range 0 1 512)

   

   ;--- distribution of wavelet coefficients
   (def WD  (c-fwt sine wavecoefs :periodic? per?))
   (def p (plot-wd wd))
   (send p :title "True Sinusoid Wavelet Coefs")

   (format t "Proportion coefs > .1 = ~5,2f~%" 
           (/ (length (setf big (which (> (abs wd) .1)))) n))
   (format t "Proportion coefs > 1 = ~5,2f~%" 
           (/ (length (which (> (abs wd) 1))) n))

   (histogram (log (abs (select (abs wd) big))))

|#
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;
   ;   Define "truth" global, add N(0,1), compute wavelet decomp, do plots
   ;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
#|

   ; --- choose true signal
   (def TRUTH bb)
   (def DATA  (+ truth (normal-rand n)))

   ; (def p (plot-wd (c-fwt truth wavecoefs :periodic? per?)))
   ; (send p :title "True Wavelet Coefs")

   ; --- generate data and compute wavelet decomp
   (def WD    (c-fwt data wavecoefs :periodic? per?))
 
   ; --- plot bit function and determine minimum
   (def values (mapcar #'(lambda (thresh) 
                           (third (ess thresh data wd)))
                       (setf x (rseq .1 2 30))))
   (def minSS (min values))
   (def bits  (/ (- values minSS) (log 2)))  ; scaleing?
   (def p (plot-points x bits :variable-labels '("Threshold" "Bits")))
   (format t "Minimum ~a at ~8,3f~%" 
           (min bits) (select x (position (min bits) bits)))


   ; --- joint plot
   (def wd (c-fwt data wavecoefs :periodic? t))
   (parameter-bits wd 'aic 2)
   (parameter-bits wd 'bic 2)

   (plot-criterion-on-threshold data 'ric 1 5)

   (plot-criterion-on-numvars   data 'ebic 1 51 20)

   ; --- modify plot

   (send plot :range 0 0 45)

   (send plot :range 1 0 4000)
   
   (send plot :size 400 400)


|#
