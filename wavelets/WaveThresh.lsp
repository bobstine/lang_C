;;;;  Wavelet thresholding

#|
   17 Dec 96 ... Start second pass for Biometrika draft.
   22 Sep 96 ... Created for model selection examples.
   16 Jan 97 ... Install the C versions of the coders.
|#

; --- get basic wavelet functions
  (require "wavecalc")

; --- get bit-length functions
  (require "intcodes")

; --- for the gs search used to find optimal threshod
  (require "search") 

; --- load the C code  (gens an error, but its ok)
  (load "coderes")
  (send *code-manager* :load 'wavelet)

  ; (send *code-manager* :unload 'wavelet)

(provide "WaveThresh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  ;;
;;   Wave-Thresh Examples  : Sine   ;;
;;                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
   ; --- Plot sine test data
   (def n         1024) 
   (def c1 4) (def c4 2)
   (def data (+ (setf signal 
                      (+ (* c1 (sin (rseq 0 (* 2 pi) n)))
                         (* c4 (sin (rseq 0 (* 8 pi) n)))))
                (normal-rand n)))
   (def p   (plot-points (iseq n) data))
   (send p :add-lines (iseq n) signal :color 'magenta)
   (send p :title (format nil "Sine: ~d obs with coefs ~d & ~d" n c1 c4))

   ; --- Smooth with lowess
   (send p :add-lines (setf smth (lowess (iseq n) data)) :color 'blue)
   (format t "MSE data ~8,2f  Lowess   ~8,2f ~%"
           (sum (^ (- signal data) 2))  
           (sum (^ (- signal (second smth)) 2)))

   ; --- By hand analysis
   ; (def wd (c-fwt data waveCoefs :periodic? t))
   ; (def kp (plot-lines (kernel-dens wd)))
   ; (send kp :add-points wd (repeat (* .05 (second (send kp :range 1))) n))
   ; (def wd^ (ebic (abs (round wd))))

   ; --- Add wavelet fit and compute MSE
   (dolist (m wave-thresh-methods)
           (let ((method m)  )
             (send p :add-lines (iseq n)
                   (+ 3 (* 3 (1+ (position m (reverse wave-thresh-methods))))
                      (setf fit (wave-compress data waveCoefs method :se 1)) )
                   :color 'green)
             (format t " MSE ~4a ~8,2f ~%"
                     method (sum (^ (- signal fit) 2)))
             ))
    (send p :adjust-to-data)


    (plot-threshold-mse data signal .01 10)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;;   Wave-Thresh Examples  : Chosen distribution for coefs   ;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
   ;--- Coefficients from some dist
   (DEF N  512)
   (def wd (combine 
            (repeat 0 2)                    ; leave first two at zero
            (* (binomial-rand (- n 2) 1 .5) ; zero some percentage of coefs
               (cauchy-rand (- n 2))
               )))
   (def DATA (c-ifwt wd wavecoefs :periodic? per?))

   ;--- plot illustrative example
   (def p  (plot-points (iseq (length data)) DATA))

   ;--- plot wavelet coef
   (plot-wd wd)

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 ;;
;;   Wavelet-Thresholding Code     ;;
;;                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def WAVE-THRESH-METHODS '(aic bic ric ebic)) ; a-bic 

(defun PLOT-THRESHOLD-MSE (data signal low high)
  ; Plots MSE of reconstrcution as function of threshold.
  ; (plot-threshold-mse data (select signal index) 0 1)
  ; 23 Dec 96
  (let ((wd   (c-fwt data waveCoefs :periodic? t))  )
    (flet ((recon (x) 
                  (/ (sum (^ (- signal 
                                (c-ifwt (apply-threshold wd x) waveCoefs))
                             2))
                     (length data)))   )
      (def tp (plot-function #'recon low high))
      (send tp :variable-label 0 "Threshold")
      (send tp :x-axis t t 5)
      (dolist (m '(aic bic ric))
              (let ((thresh (threshold m (length data) :parmLen 64))  )
                (send tp :add-lines (repeat thresh 2) (send tp :range 1))
                ))
      )))

(defun OPTIMAL-THRESHOLD-MSE (data signal low high)
  ; Finds reconstruction as function of threshold.
  ; (plot-threshold-mse    data (select signal index) 0 5)
  ; (optimal-threshold-mse data (select signal index) 0 5)
  ; 7 Jan 97
  (let ((wd   (c-fwt data waveCoefs :periodic? t))  )
    (flet ((recon (x)  ; negative since gs finds the max
                  (- 
                   (sum (^ (- signal 
                              (c-ifwt (apply-threshold wd x) waveCoefs))
                           2))
                   )))
      (* '(1 -1) (golden-search #'recon 0 10))
      )))


(defun APPLY-THRESHOLD (wd threshold)
  ; Apply hard threshold to wavelet coefs
  ; 23 Dec 96
  (mapcar #'(lambda (x) (if (< (abs x) threshold) 0 x))
          wd)
  )

#| Data is 1000 sample bb+noise gened by sim-bb from WaveSim.lsp
   Net gain from switch to C is about 2 out of 2.4 seconds for aic.

   (time (def res (wave-compress data *d4* 'aic)))

|#

(defun WAVE-COMPRESS (data waveCoefs method &key (se 1))
  ; Choose wavelet coefficients based on associated criterion
  ; implemented with bit counting.  
  ; Returns (#bits #non-zero-coefs (fit))
  ; 18 Dec 96 ... 23 Dec 96 ... 16 Jan 97
  (if (eq 'ls method)  data
      (let* ((n        (length data))
             (wd       (c-fwt data wavecoefs :periodic? t))
             (compress (wavelet-coder method wd
    ;        (compress (funcall (symbol-function method) wd
                               :n (length wd)
                                :se se
                                :parmLen 1
                                ))
             (wd^      (second compress))     )
        (list (first compress)                      ; bits
              (- n (count-if #'zerop wd^))          ; number not zero
              (c-ifwt wd^ waveCoefs :periodic? t)   ; fit
              )
        )))


(defun THRESHOLD (method n &key (parmLen 0))
  ; Rough threshold approximations from asymptotics.
  ; 23 Dec 96
  (if (member method '(aic bic ric))
      (case method
        ('aic   (* 2 (sqrt (* 2 (log 2)))))
        ('bic   (sqrt (+ (log n) (* 2 (log parmLen)))) )
        ('ric   (sqrt (* 2 (log n))))    )
      (format t "Method ~a not available.~%")
      ))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;                                         ;;
;;   C CODERS                              ;;
;;      Input  - raw z score (not rnded)   ;;
;;      Return - (rounded theta,           ;;
;;                total bit contribution)  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#| SORTING KILLS YOU for long series, so avoid it. For length 500
   the c versions take about 0.02 seconds while lisp version take
   at least 1.30 sec... 60 times faster.  For ebic, this improvement
   is less impressive since both have to sort the sequence.

   (setf z (rseq -10 10 501))
   (time (def cres (wavelet-coder 'ebic z :eachBits t)) )
   (time (def  res (ebic z :n (length z) )) )

   (scatterplot-matrix 
    (remove-if #'endp (append (list z) (rest res) (rest cres))))
|# 

(defun WAVELET-CODER (method zScores &key (parmLen 1) eachBits)
  ; Current n is forced to number of z's and only se=1.
  ; parmlen only useful for BIC method.
  ; 17 Jan 97
  (let* ((name  (format nil "~a" method))
         (n     (length zScores))
         (args  (list (float zScores)    n    ; 0 1
                      (repeat -7 n)           ; 2     codes
                      (if eachBits 1 0)       ; 3
                      (float -777.7)))        ; 4     total bits
         (xtra   (case method
                   ('bic   (float parmLen))
                   ('ebic  (order (- (abs zScores))))   ))
         (res    (apply #'call-cfun name
                        (if xtra (append args (list xtra))
                            args)))   )
    (if eachBits
        (list (first (select res 4)) (select res 2) (first res))
        (list (first (select res 4)) (select res 2))
        )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;                                         ;;
;;   LISP CODERS                           ;;
;;      Input  - raw z score (not rnded)   ;;
;;      Return - (rounded theta,           ;;
;;                total bit contribution)  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; utilities

(let ((2log2 (* 2 (log 2)))  )
  (defun DATABITS (z)
    (/ (^ z 2) 2log2)
    ))

(defun PLOT-CODER (coder zScores)
  ; (plot-coder #'ric  (rseq 0 10 10))
  ; 7 Jan 97
  (let ((codes (funcall coder zScores))  )
    (if (and (boundp 'coderBitPlot) (send coderBitPlot :allocated-p))
        (progn (send coderBitPlot :add-lines zScores (third codes))
               (send coderBitPlot :adjust-to-data))
        (setf coderBitPlot (plot-points zScores (third codes))))
    ))

(defun PRINT-CODER (coder zScores)
  ; (print-coder #'aic  (rseq 0 5 30))
  ; 7 Jan 97
  (let ((codes (funcall coder zScores))  )
    (format t "Total coder bits: ~d = ~d~%" (first codes) (sum (third codes)))
    (print-matrix (apply #'bind-columns 
                         zScores 
                         (if (third codes) 
                             (rest codes)
                             (butlast (rest codes)))  ))
    ))

;;;; given z, coders return 
;;;;               total bits
;;;;               list of theta^
;;;;               list of contributions to the total bit length
         
#|
  (c-b
  (plot-coder  #'(lambda (z) (bic z :n 1024 :parmLen 4)) (rseq 0 10 50))
  (print-coder #'(lambda (z) (bic z :n 1024 :parmLen 4  :se 2)) 
               '(-1.8 0 1.5 1.8 1 2 3 4 7 12 20))
|#


(defun BIC (zScores &key (n 0) (parmLen 1) (se 1))
  ; 22 Dec 96 ... 7 Jan 97
  (let* ((codeBits (floor (1+ (* .5 (log (* (1- n) (^ (/ parmLen se) 2)) 2)))))
         (pairs    (transpose
                   (mapcar 
                    #'(lambda (z)
                        (let* ((rz   (* se (round (/ z se))))
                               (zero (+ 1 (databits z)))     ; + 1 for hypothesis
                               (code (+ 1 codeBits (dataBits (- z rz))))   )
                          (if (< zero code)
                              (list 0   zero)
                              (list rz  code)
                              )))
                    zScores))
                  ))
    (cons (sum (second pairs)) pairs)
    ))
        

(defun AIC (zScores &key (se 1))
  ; (plot-coder  #'(lambda (z) (aic z)) (rseq 0 10 100))
  ; (print-coder #'(lambda (z) (aic z)) '(-1.89 -1.8 0 1.8 1.88 3 7 12 20))
  ; (def a (aic (rseq -1 100 200) :se 1))
  ; (def a (aic '(-1.8 -1.5 0 1.5 1.8 2 3 7 12 20) :se 2))
  ; 22 Dec 96 ... 9 Jan 97
  (let* ((try '(0 1 2))
         (pairs (transpose
                 (mapcar 
                  #'(lambda (z)
                      (let* ((rz    (if (< 0 z)
                                        (- (round (/ z se)) try)
                                        (+ (round (/ z se)) try))  )
                             (data  (dataBits (- z (* se rz))))
                             (code  (mapcar #'signed-cauchy-bit-length (abs rz)))
                             (total (+ data code)) 
                             (j     (position (min total) total))   )
                        (list (* se (select rz j))
                              (select total j))
                        ))
                  zScores))
                ))
    (cons (sum (second pairs)) pairs)
    ))

#|
  (plot-coder  #'(lambda (z) (ric z :n 1024)) (rseq 0 10 50))
  (print-coder #'(lambda (z) (ric z :n 1024)) 
               '(-1.8 0 1.5 1.8 1 2 3 4 7 12 20))
|#

(def RIC-BIT-COLUMN t)   ; add the individual bit column?
                
(defun RIC (zScores &key (se 1))
  ; Like aic, but with option of using zero bits for zScore
  ; Cannot map since done sequentially from largest |z|
  ; Parm bits include 1 for continuation bit + length of index
  ; (first (setf r (ric    '(-10 -1.8  1.8 2 3 7 12) :se 1)))
  ; (def r (ric (combine (repeat 0 40) '(1 3 7 12)) :se 2))
  ; (plot-coder #'ric (rseq 0 10 100)) (plot-coder #'aic (rseq 0 10 20))
  ; 22 Dec 96 ... 7 Jan 97
  (let* ((aicCodes    (second (aic zScores :se se)))
         (sortIndex   (order (- (abs zScores))))         ; descending
         (dataBits    (databits (- aicCodes zScores)))
         (cumDataBits (accumulate #'+ (select dataBits sortIndex)))
         (p           (length zScores))
         (indexBits   (integer-bit-length (1- p)))
         (parmBits    (+ 1 indexBits (mapcar #'signed-cauchy-bit-length aicCodes)))
         (cumParmBits (accumulate #'+ (select parmBits sortIndex)))
         (cumCodeBits (cons 0 (+ cumDataBits cumParmBits)))
         (cumZeroBits (reverse                             ; code rest as zero
                       (cons 0
                             (accumulate
                              #'+ 
                              (reverse (dataBits (select zScores sortIndex)))))))
         (cumTotBits (float (+ cumCodeBits cumZeroBits)))
         (minBits    (min cumTotBits))
         (minIndex   (position minBits cumTotBits)) 
         (index      (select sortIndex (iseq minIndex)))
         (useZ       (repeat 0 p))   )
    (setf (select useZ index) (select aicCodes index))
    (list minBits useZ
          (when ric-bit-column
                (let ((bits  (repeat 0 p))
                      (other (if (< minIndex p)
                                 (select sortIndex (iseq  minIndex (1- p))) nil)) )
                  (when index
                        (setf (select bits index)(+ (select parmBits index)
                                                    (select dataBits index))))
                  (when other
                        (setf (select bits other)(databits (select zScores other))))
                  bits)
                )
          )))
  
#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  EBIC Group of functions

   (defun pr (x)
     (format t "Print - ~a~%" x)
     x)

   (print-coder #'ebic  '(-1.8 -1.6 0 1.8 1.9 12 12 12))

   (let* ((z     '(-1.8 -1.8 0 1.8 1.9 12))
          (place (reverse (order (abs z))))  )
     (format t "place is ~a~%" place)
     (def a (aic-for-ebic z place :se 1))   )

; add these lines for printing intermediate results
     (junk  (format t "~4,1f:~{ ~5d ~}~%        ~{ ~5,2f ~}~%        ~{ ~5,2f ~}~%" 
                  z rz code total))

 |#


(defun AIC-FOR-EBIC (zScores order &key (se 1))
  ; 7 Jan 97 ... 9 Jan 97
   (let* ((try   '(-1 0 1 2))   ; may need to expand
         (zero?  nil)
         (j      -1)
         (p      (length zScores))
         (zCodes (repeat 0 p))     )
    (dotimes (i p)
             (setf j (select order i))
             (let* ((z      (select zScores j))
                    (rz     (if (< 0 z)
                                (- (round (/ z se)) try)
                                (+ (round (/ z se)) try))  )
                    (data   (dataBits (- z (* se rz))))
                    (code   (ebic-parm-lengths rz (1+ i) p))
                    (total  (+ data code))
                    (minPos (position (min total) total :test #'=)) 
                    (code   (* se (select rz minPos)))   )
               (when (= 0 code) (return))
               (setf (select zCodes j) code)
               ))
     zCodes))

(defun EBIC-PARM-LENGTHS (z k n)
  ; Compute coef length contributions given ebic coding
  ; 9 Jan 97
  (let ((bits (1- (mapcar #'signed-cauchy-bit-length (abs z)))) ; one bit in a_i
        (pos  (position 0 z :test #'=))  )
    (if pos ; zero is scored relative to others
        (let* ((prop (min .9999 (max .0001 (/  k n))  ))
               (entDiff (- (log (- 1 prop) 2)
                           (log      prop  2)))  )
          ; (format t "Input z=~a with p = ~d/~d, dec zero ~5,2f bits" z k n entDiff)
          (decf (select bits pos) entDiff)))
    ; (format t " gives ~a~%" bits)
    bits
    ))

(def EBIC-BIT-COLUMN t)   ; add the individual bit column?

(defun EBIC (zScores &key (se 1))
  ; Done sequentially from largest |z|; use switch to compute sep bits
  ; (print-coder #'aic  '(0 0 0 0 1.5 12 12 12 12 12 ))
  ; (print-coder #'(lambda (z) (ebic z)) '(-1.8 -1.8 0 0 1.8 1.9))
  ; (print-coder #'ebic '(0 0 0 0 1.7 12 12 12 12 12 ))
  ; 18 Dec 96 ... 29 Dec 96 ... 7 Jan 97
  (let* ((p           (length zScores))
         (sortIndex   (order (- (abs zScores)))) ; descending
         (aicCodes    (aic-for-ebic zScores sortIndex :se se))
         (dataBits    (databits (- aicCodes zScores)))
         (cumDataBits (accumulate #'+ (select dataBits sortIndex)))
         (parmBits    (mapcar #'cauchy-bit-length aicCodes))  ; sign compressed
         (cumParmBits (+ (* p (mapcar #'h (/ (iseq 1 p) p)))  ; entropy location
                         (accumulate #'+ (select parmBits sortIndex))  ))
         (cumCodeBits (cons 0 (+ cumDataBits cumParmBits)))
         (cumZeroBits (reverse 
                       (cons 0 ; code rest as zero
                             (accumulate
                              #'+ 
                              (reverse (dataBits (select zScores sortIndex)))))))
         (cumTotBits (float (+ cumCodeBits cumZeroBits)))
         (minBits    (min cumTotBits))
         (minIndex   (position minBits cumTotBits))
         (index      (select sortIndex (iseq minIndex)))
         (useZ       (repeat 0 p))
         )
    (setf (select useZ index) (select aicCodes index))
    (list (+ minBits (integer-bit-length (1- p))) 
          useZ
          (when ebic-bit-column
                (let ((bits  (repeat 0 p))
                      (prop  (/ minIndex p))
                      (other (if (< minIndex p)
                                 (select sortIndex (iseq  minIndex (1- p))) nil)) )
                  (when index
                        (setf (select bits index) 
                              (+ (- (log prop 2))
                                 (select parmBits index)
                                 (select dataBits index))))
                  (when other
                        (setf (select bits other)
                              (+ (- (log (- 1 prop) 2))
                                 (databits (select zScores other)))))
                  bits)
                )
          )))

(defun OLD-EBIC (zScores &key (se 1))
  ; Cannot map since done sequentially from largest |z|
  ; (def e (old-ebic (rseq 0 25 100) :se 1))
  ; (first (setf a (aic    '(-1.8 -1.5 0 1.5 1.8 2 3 7 12) :se 1)))
  ; (first (setf m (m-ebic '(-1.8 -1.5 0 1.5 1.8 2 3 7 12) :se 1)))
  ; 18 Dec 96 ... 29 Dec 96
  (let* ((aicCodes    (second (aic zScores :se se)))
         (sortIndex   (order (- (abs zScores)))) ; descending
         (cumDataBits (accumulate
                       #'+ (select (databits (- aicCodes zScores)) sortIndex)))
         (cumParmBits (accumulate  ; Cauchy length since compressed lead
                       #'+ (select (mapcar #'cauchy-bit-length aicCodes)
                              sortIndex)))
         (p           (length zScores))
         (cumCodeBits (cons 0
                            (+ (ceiling (* p (mapcar #'h (/ (iseq 1 p) p))))
                               cumDataBits cumParmBits)))
         (cumZeroBits (reverse
                       (cons 0
                             (accumulate
                              #'+ 
                              (dataBits 
                               (select zScores (reverse sortIndex)))))))
         (cumTotBits (float (+ cumCodeBits cumZeroBits)))
         (minBits    (min cumTotBits))
         (useZ       (repeat 0 p))
         (index      (select sortIndex (iseq (position minBits cumTotBits))))
         )
    ; (def plot (plot-points (iseq (1+ p)) (+ cumCodeBits cumZeroBits)))
    (setf (select useZ index) (select aicCodes index))
    (list minBits useZ)
    ))



;;;;;;;;;;;;;;;


(defun PLOT-CRITERION-ON-THRESHOLD (data method low high &key (points 15))
  ; Plots residual SS (red), MSE (magenta), Parameters (blue) for thresholds
  ; in the indicated [low,high] range.  Always shows LS and AIC points.
  ; 25 Sep 96
  (let* ((wd   (c-fwt data wavecoefs :periodic? t))
         (thresholds  (rseq low high points))
         (parm (mapcar #'(lambda (th) (parameter-bits wd method th)) thresholds))
         (fits (transpose (mapcar #'(lambda (th) (ess th data wd)) thresholds)))
         (rss  (second fits))
         (rss  (/ (- rss (min rss)) (* 2 (log 2))))
         (mse  (third fits))
         (mse  (/ (- mse (min mse)) (* 2 (log 2))))  )
    (setf plot (plot-lines thresholds parm :color 'blue
                           :variable-labels '("Threshold" "Bits")))
    (send plot :title (format nil "~a" method))
    (send plot :add-lines thresholds rss :color 'red) ; residual ss as bits
    (send plot :add-lines thresholds (* 5 mse) :color 'green) ; expand scale
    (send plot :add-lines thresholds (+ parm rss) :color 'magenta)
    (send plot :adjust-to-data)
    (list thresholds rss mse parm)
    ))
       

(defun PLOT-CRITERION-ON-NUMVARS (data method low high use)
  ; Plots residual SS (red), MSE (magenta), Parameters (blue) for thresholds
  ; in the indicated [low,high] range.  Always shows LS and AIC points.
  ; 25 Sep 96
  (let* ((wd    (c-fwt data wavecoefs :periodic? t))
         (nvar  (round (rseq low high use)))
         (thrsh (select (reverse (sort-data (abs (cddr wd)))) nvar))
         (parm  (mapcar #'(lambda (th) (parameter-bits wd method th)) thrsh))
         (fits  (transpose (mapcar #'(lambda (th) (ess th data wd)) thrsh)))
         (rss   (second fits))
         (rss   (/ (- rss (min rss)) (log 2)))
         (mse   (third fits))
         (mse   (/ (- mse (min mse)) (log 2)))  )
    (format t "Number variables: ~a ~%" nvar)    
    (format t "Thresholds are: ~{ ~6,2f ~} ~%" thrsh)
    (setf plot (plot-lines nvar (- parm 512)  :color 'blue ; for dean
                           :variable-labels 
                           '("Number of Variables" "Penalized Log Like")))
    (send plot :title (format nil "~a" method))
    (send plot :add-lines nvar rss :color 'red) ; res ss
    (send plot :add-lines nvar mse :color 'magenta)
    (send plot :add-lines nvar (+ parm rss) :color 'magenta)
    (send plot :adjust-to-data)
    (list nvar thrsh rss mse parm)
    ))
       

(defun PARAMETER-BITS (wd method thresh)
  ; Counts bits for wavelet parameters after thresholding, assuming sigma 1
  ; (mapcar #'(lambda (th) (parameter-bits '(.1 1 2 3 4) 'aic th)) '(.5 1.5))
  ; 25 Sep 96 ... 9 Dec 96
  (let* ((n      (length wd))
         (big    (select wd (which (<= thresh (abs wd))))) ; find big ones
         (nZeros (- n (length big)))                       ; count zeros
         )
    ; Set the wavelet coefs which are below threshold to zero.
    (setf (select wd (which (> thresh (abs wd)))) (repeat 0 nZeros))
    (format t "~d coefs below ~5,2f zeroed for coding.~%" (length big) thresh)
    ; Now compute the bits (as rounded integer) for chosen method
    (ceiling
     (case method
       ;       Least squares codes all of the coefficients, ignoring zeros
       ('ls    (parameter-bits wd 'bic 0))
       ;       AIC uses cauchy code for each rounded to integer
       ('aic   (sum  (mapcar #'signed-cauchy-bit-length (round wd))))
       ;       BIC uses uniform code for rounded big ones
       ('bic   (+ n  (* (length big) .5 (log n 2))))
       ;       Adaptive BIC uses an efficient code for prefix
       ('a-bic (+ (* n (h (/ nZeros n)))
                  (* (length big) .5 (log n 2))))
       ;       RIC/hard adds length for indices
       ('ric   (+ (* (length big) (log n 2))  
                  (sum (mapcar #'signed-cauchy-bit-length (round big)))  ))
       ;       EBIC uses efficient prefix code
       ('ebic  (+ (* n (h (/ nZeros n)))
                  (sum (mapcar #'signed-cauchy-bit-length (round big)))  ))
       ))))


(defun ESS (thresh data wd)
  ; Returns list of threshold, residual ss and error ss about "truth"
  ; given input decomposisiton.  Requires global data, per?,wavecoefs,truth
  (let*  ((wdt (mapcar #'(lambda (x) (if (< (abs x) thresh) 0 x))
                       wd))
          ( y^ (c-ifwt wdt wavecoefs :periodic? per?)))
    (list thresh
          (sum (^ (- data y^) 2))
          (sum (^ (- truth y^) 2))
          )))
   
   ; (ess .5 data wd)

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;   Example of finding which bases have support in "middle half"
;                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

   ;--- Define parameters
   (def n     512)
   (def waveCoefs *d4*)
   (def per?  t)     ; Use periodic coefs?

   ; --- Pick the wavelet coefficient which is non-zero
   (def index  (+ (^ 2 5) 3))
   (def testData   (repeat 0 n))  (setf (select testData index) 1)

   ; --- Invert the transform and check the support
   (def wave (c-ifwt testData wavecoefs :periodic? per?))
   (format t "Number not zero is ~d~%" (- n (count-if #'zerop wave)))
   (def p (plot-points (iseq n) wave))
   (send p :title (format nil "Wavelet index ~d" index))

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                          ;;
;;   Extra code                             ;;
;;                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun H(p)
  ; Entropy of bernouilli
  (cond 
    ((zerop p) 0)
    ((= 1 p)   0)
    ( t       (let* ((1-p (- 1 p))
                     (ent (- (+ (* p (log p 2))
                                (* 1-p (log 1-p 2)))  ))  )
                ; (format t " ~8,4f & ~8,4f \\cr ~%" p ent)
                ent))
    ))

(defun TIME-PLOT (x &optional (label " "))
  (let* ((n (length x))
         (time (rseq 0 1 n))
         (p (plot-points time x :variable-labels
                         (list "Time" label)))
         )
    (send p :add-lines time x)
    p
    ))

(defun TIE-DOWN (xt)
  ; If xt is weiner process, converts to bb
  ; Assumes time interval is [0,1].
  ; 18 Dec 96
  (- xt (* (rseq 0 1 (length xt)) (first (last xt))))
  )

(defun QQ-PLOT (x &optional (label "data"))
  (let ((n (length x))  )
    (plot-points 
     (normal-quant (/ (iseq 1 n) (1+ n)))
     (sort-data x) 
     :variable-labels (list "Normal" label))
    ))
  

;;;;
