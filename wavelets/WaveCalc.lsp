#|

                        WaveCalc.LSP

  25 Jul 95 ... Form objects from this code.
  19 May 95 ... Enhance with decimation options.
   4 May 95 ... Lisp code to experiment with wavelet transforms.

|#

(require "WaveCoef")

#|;------------------------------------------------------------------------


        Data spike

   ; --- Data series
   (def n 128)
   (def time (iseq n))
   (def data (combine (repeat 0 27) '(1) (repeat 0 100)))
   ; --- calculate wavelets and plot (using c code prepend c-)
   (def wd   (fwt data *d4* :periodic? t))
   (plot-wd wd)
   

        Thresholding of Wavelets

   ; --- Generate test data
   (def n    256)
   (def signal (combine (repeat 1 128) (repeat 0 128)))
   ; (def signal (sin (rseq 0 (* 2 pi) n)))
   (def y (+ signal (* .3 (normal-rand n))))

   ; --- Initial plot
   (def p (plot-points (iseq n) signal :color 'magenta :symbol 'dot4))
   (send p :add-points (iseq n) y)
   (send p :adjust-to-data)

   ; --- Wavelet decomposition
   (def coefs *d4*)
   (def per t)
   (def wd    (fwt y coefs :periodic? per))
   (plot-wd wd)
   (def cwd (c-fwt y coefs :periodic? per))
   (plot-wd cwd)

   ; --- Animated reconstruction plot
   (dynamic-reconstruction coefs wd p :periodic? per)

   ; --- Plots of decomposition 
   ; (mapcar #'standard-deviation (rest wd)) ; first is special
   (boxplot (cddr wd))
   (plot-wd wd)

   ; --- Thresholding
   (boxplot (rest wd))
   (plot-wd
    (setf wdt (threshold-wd wd 1 'hard  :sigma 1 :skip 1))   )
   
   ; --- Wavelet reconstructions without/with thresholding
   ; (send p :add-points (iseq n) (wavelet-reconstruction coefs wd)
   ;       :symbol 'cross)  ; shows effects of boundaries
   (send p :add-lines (iseq n) (ifwt wdt coefs)
         :color 'blue)

   ; --- Draw the wavelet
   (setf temp (reverse
               (let ((w (list (vector 0.0)))  )
                 (dotimes (j 10)
                          (push (make-array (^ 2  j) :initial-element 0.0)
                                w))
                 w)))
   (setf level (select temp 3))
   (setf (aref level (/ (length level) 2)) 10.0)
   (setf wave (wavelet-reconstruction *d8* temp :periodic? nil))
   (plot-lines (iseq (length wave))  wave)  ; - reverse ?
         
         
|#;------------------------------------------------------------------------
         

(defun DYNAMIC-RECONSTRUCTION (coefs wd plot &key periodic?)
  ; Shows the reconstructed series as animated with a slider.
  ; 15 May 95
  (let* ((sd   (standard-deviation (select wd (1- (length wd)))))
         (y    (ifwt wd coefs :periodic? periodic?))
         (x    (iseq (length y)))
         (vp?  (send plot :allocated-p))
         (y^   nil)
         (cp   (plot-wd wd))
         )
    (send cp :location 0 250)
    (when vp? (send plot :add-lines x y))
    (interval-slider-dialog
     (list 0.0 (* 12 sd))
     :points 50
     :action #'(lambda (threshold)
                 (send plot :clear-lines)
                 (let ((wdt (threshold-wd wd threshold 'hard :skip 1))  )
                   (when vp?
                         (send plot :add-lines x
                               (setf y^ (ifwt wdt coefs :periodic? periodic?)))
                         )
                   (format t "RSS = ~8,3f" (sum (^ (- y y^) 2)))
                   (if (boundp 'truth)
                       (format t " SSE = ~8,3f" (sum (^ (- truth y^) 2))))
                   (send cp :clear-lines)
                   (plot-wd wdt :addto cp)
                   )))
    ))


(defun PRINT-WD (wd &optional label)
  (if label (format t "~a~%" label))
  (format t "Phi[ 0]  ~7,2f~%" (elt (first wd) 0))
  (mapcar #'(lambda (i c)
              (format t "Psi[~2d] ~{ ~7,2f ~} ~%"
                      i (coerce c 'list)))
          (iseq (1- (length (rest wd))))
          (rest wd))
  )


(defun PLOT-WD (wd &key addTo)
  ; WD is wavelet decomp, starting at most coarse stage.
  ; Will convert to a nested list if wd comes as combined list.
  ; 15 May 95 ... 29 Jul 95
  (unless (vectorp (third wd))  
          (setf wd (nested-binary-list wd)))
  (let* ((wd    (rest wd))  ; drop scaling coef
         (depth (+ 2 (- (length wd))))
         (sd    (* 3 (standard-deviation (rest wd))))
         (wd    (reverse (rest (/ wd      ; rescale, start with fine coefs
                                  (if (zerop sd) 1 sd)))))
         (color 'black)
         (scl   2)    ; double x spacing for each
         ( p    (if addto addto
                    (plot-lines (list 0 (* 2 (length (first wd)))) '(0 0)
                                :variable-labels (list "Time" "Depth"))))   )
    (format t "Scaling for plot is 1=~7,3f~%" sd)
    (dolist (w wd)
            (setf color (if (eq color 'black) 'blue 'black))
            (send p :abline depth 0)
            (mapc #'(lambda (x pt)
                      (send p :add-lines (repeat x 2) (list depth pt)
                            :color color))
                  (+ 2 (* scl (iseq (length w))))  (coerce (+ w depth) 'list))
            (setf depth (1+ depth)) 
            (setf scl (* 2 scl))
            )
    (unless addto (send p :adjust-to-data))
    p
    ))



(defun COPY-DECOMPOSITION (wd)
  ; Makes a copy using appropriate internal format (C or Lisp convention)
  ; 15 May 95 ... 22 Sep 96 ... 9 Jan 97
  (if (arrayp (first wd))  ; lisp output
      (mapcar #'copy-vector wd)  ; was a rest here ???
      (copy-list wd)
      ))


(defun NESTED-BINARY-LIST (wdList)
  ; Converts a list decomposition into a nested list
  ; (nested-binary-list (iseq 8))
  ; 18 Oct 95
  (labels ((bin (x j)
                (let ((n (length x)))
                  (if (= n j)
                      (list x)
                      (cons (select x (iseq j))
                            (bin (select x (iseq j (1- n))) (* 2 j))
                            )))))
    (cons (list (first wdList)) (bin (rest wdList) 1))
    ))


(defun WD-SCALE (wd)
  ; Uses either last block or the last half of the coefs.
  ; ... 22 Sep 96
  (if (arrayp (first wd))      ; lisp version output
      (standard-deviation (first (last wd)))
      (let ((n (length wd))  )
        (standard-deviation (select wd (iseq (/ n 2) (1- n)))))
      ))


(defun THRESHOLD-WD (wd threshold type &key (skip 1) sigma)
  ; Apply thresholding to wavelet coefs below threshold, skipping skip levels.
  ; Works for the nested list format only.
  ; By default, skips the scaling overall coefficient (leading term)
  ; Returns a copy rather than in-place.
  ; 15 May 95 ... 30 Jul 95
  (let* ((bound (* threshold (if sigma sigma (wd-scale wd))))
         (new   (copy-decomposition wd))   )
    ; (format t "~a thresholding at ~6,2f = ~6,2f x ~6,2f (sigma)~%"
    ;         type bound threshold sigma)
    (if (eq type 'hard)
        (dolist (w (select new (iseq skip (1- (length wd)))))
                (dotimes (j (length w))
                         (when (< (abs (aref w j)) bound)
                               (setf (aref w j) 0.0))
                         ))
        (dolist (w (select new (iseq skip (1- (length wd)))))
                (dotimes (j (length w))
                         (setf (aref w j)
                               (if (< (abs (aref w j)) bound)
                                   0.0
                                   (if (< (aref w j) 0)
                                       (+ (aref w j) bound)
                                       (- (aref w j) bound))
                                   ))))
        )
    new))



#|;------------------------------------------------------------------------

   (print-matrix (filter-matrix '(c0 c1 c2 c3)))
   (print-matrix (filter-matrix '(c0 c1) :dim 6))
   (print-matrix (filter-matrix '(c0 c1 c2 c3) :dim 10))
   (print-matrix (filter-matrix '(1 2 3 4) :dim 8 :hi? t))

   (print-round-mat (setf L (filter-matrix *d4* :dim 8)))
   (setf LP (periodic-filter-matrix *d6* :dim 8))

   (print-round-mat (setf H (filter-matrix *d4* :dim 8 :hi? t)))
   (setf HP (periodic-filter-matrix *d6* :dim 8 :hi? t))
   (print-matrix (round (* 100 lp)))
   (print-matrix (round (* 100 hp)))

   ; --- use to standardize row SS
   (dotimes (i 4) ; for each row
            (let ((ss 0))
              (dotimes (j 8)
                       (incf ss (^ (aref l i j) 2)))
              (setf ss (sqrt ss))
              (dotimes (j 8)
                       (setf (aref l i j) (/ (aref l i j) ss))
                       )))
                             

   ; --- Although orthogonal, coefs are not normalized at boundary
   (print-matrix (matmult H (transpose L)))   ; orthogonal
   (print-matrix (matmult HP (transpose LP))) ; orthogonal
   (print-matrix (matmult H (transpose LP)))  ; not orthogonal if one is wrapped

   (print-matrix (matmult H (transpose H)))   ; not orthogonal.
   (print-matrix (matmult L (transpose L)))

   (print-matrix (matmult HP (transpose HP))) ; orthogonal
   (print-matrix (matmult LP (transpose LP)))


|#;------------------------------------------------------------------------

(defun FILTER-MATRIX (coefs &key dim hi? periodic?)
  ; Returns the matrix whose eigenvalues determine the filter properties.
  ; Unfortunately, lisp-stat eigen function only works for symmetric.
  ; Defaults to minimal interesting dimension.
  ; Indexing is queer since i and j are really (1+i) and (1+j).
  ; 11 May 95
  (let* ((n   (length coefs))
         (dim (if dim dim n))
         (k 0)
         (mat (make-array (list (/ dim 2) dim) :initial-element 
                          (if (numberp (select coefs 0))
                              0 '--)))   )
    (if hi?
        (progn 
         (setf coefs (* coefs (^ -1 (iseq n))))
         (dotimes (i (/ dim 2))
                  (dotimes (j dim)
                           (let ((k (- j (* 2 i)))  )
                             (when (< -1 k n)
                                   (setf (aref mat i j) (elt coefs k)))
                             ))))
        (dotimes (i (/ dim 2))
                 (dotimes (j dim)
                          (let ((k (- (1+ (* 2 i)) j))  )
                            (when (< -1 k n)
                                  (setf (aref mat i j) (elt coefs k))
                                  ))))
        )
    mat))


(defun PERIODIC-FILTER-MATRIX (coefs &key dim hi?)
  ; Wraps in time 
  ; 16 May 95
  (let* ((nc  (length coefs))
         (dim (if dim dim (1- n)))
         (mat (make-array (list (/ dim 2) dim) :initial-element 
                          (if (numberp (aref coefs 0))
                              0 '--)))   )
    (if hi?
        (let ((c (* coefs (^ -1 (iseq nc))))  )
          (dotimes (i (/ dim 2))
                   (dotimes (j nc)
                            (incf (aref mat i (mod (+ j (* 2 i)) dim))
                                  (aref c j))))
          )
        (dotimes (i (/ dim 2))
                 (dotimes (j nc)
                          (incf (aref mat i (mod (- (* 2 (1+ i)) (1+ j)) dim))
                                (aref coefs j)))
                 ))
    mat))
                                
                          


                          
#|;------------------------------------------------------------------------

   Recursive Wavelet Decomposition

   (round (* 10000 (fwt (iseq 16) *d4* :periodic? nil)))

   (setf wd  (wavelet-decomposition '(9 1 2 0    9 1 2 0) *haar*))

   (setf nwd (wavelet-decomposition '(9 1 2 0    9 1 2 0) *haar* :decimate? nil))

   (def wd (wavelet-decomposition *d6* '(1 3 5 4 3 2 1 2 3 4 5 3 5 3 1 3)
                                  :periodic? t))

|#;------------------------------------------------------------------------

(defun FWT (data coefs &key (periodic? t))
  ; Assuming data are of length which is power of 2, returns list of hi freq
  ; wavelet coefficient pairs from underlying qmf. First special with both.
  ; 12 May 95 ... 25 Jul 95
  (let ((lim    (log (length data) 2))
        (count  -1)
        (data   (coerce data 'vector))
        (high   (wave-from-scale coefs))
        (filter (if periodic? #'periodic-qm-filter #'qm-filter))  )
    (labels ((wd (x)
                 (incf count)
                 (if (= count lim)
                     (list x)
                     (let ((qmf (funcall filter coefs high x))  )
                       (cons (second qmf)    ; just save wavelet coefs
                             (wd (first qmf))  ))))     )
      (reverse (wd data))
      )))
               


#|;------------------------------------------------------------------------

   Quadrature Mirror Filter


   (periodic-qm-filter *d4* (wave

   (qm-filter *haar* (* *haar* '(1 -1)) '(9 1 2 0))
   (qm-filter *d6* (* *d6* '(1 -1 1 -1 1 -1)) '(9 1 2 0))
   (qm-filter *d6* (* *d6* '(1 -1 1 -1 1 -1)) '(9 1 2 0 0 0 0 0))

   (qmf (reverse *haar*) (* *haar* '(1 -1)) '(9 1 2 0))
   (qmf (reverse *d6*  ) (* *d6* '(1 -1 1 -1 1 -1)) '(9 1 2 0))

   ; --- check quad-mirror calculations via matrices for D6

   (setf x '(1 3 5 4 3 2 1 2 3 4 5 3 5 3 1 3))
   (setf  qm (qm-filter *d6* (wave-from-scale *d6*) x))
   (setf nqm (qm-filter *d6* (wave-from-scale *d6*) x :decimate? nil))

   (print-round-mat (setf L  (filter-matrix *d6* :dim (length x))))
   (setf lo  (matmult L x))
   (format t "Low difference = ~a~%" (- lo (first qm)))

   (print-round-mat (setf H  (filter-matrix *d6* :dim (length x) :hi? t)))
   (setf hi  (matmult H x))
   (format t "Hi  difference = ~a~%" (- hi (second qm)))


|#;------------------------------------------------------------------------


(defun PERIODIC-QM-FILTER (c hc x &key (decimate? t))
  ; Compute qmf band-pass filter using scaling coefs c and wavelet hc
  ; 16 May 95 ... 19 May 95
  (let* ((n      (length x))
         (len    (if decimate? (/ n 2) n))
         (nc-1   (1-  (length c)))
         (xin    0)
         (zlo    (make-array len :initial-element 0.0))
         (zhi    (make-array len :initial-element 0.0))  )
    ; (format t "per-qmf: Using length ~d~%" len)
    (dotimes (j len)
             (setf xin (if decimate? (* 2 j) (1- j)))
             (setf (aref zlo j) 
                   (inner-product c
                                  (select x (mod 
                                             (iseq (1+ xin) (- (1+ xin) nc-1))
                                             n)) ))
             (setf (aref zhi j)
                   (inner-product hc 
                                  (select x (mod 
                                             (iseq xin (+ xin nc-1))
                                             n))  ))
             )
    (list zlo zhi)
    ))



(defun  QM-FILTER (c hc x &key (decimate? t))
  ; Compute qmf band-pass filter using scaling coefs c and wavelet hc applied
  ; to positive 2^k length vector data series x.  Faster with reversed x than
  ; by using tail indexing.
  ; 11 May 95 ... 19 May 95
  (let* ((xr     (reverse x))                ; reverse data 
         (n      (length x))
         (len    (if decimate? (/ n 2) n))   ; length of result
         (nc     (length c))
         (zlo    (make-array len :initial-element 0.0))
         (zhi    (make-array len :initial-element 0.0))
         (edge   (1- (if decimate? (/ nc 2) nc)))
         (xin    (if decimate? 1 0))
         (xincr  (if decimate? 2 1))   )
    (format t "nonper-qmf: Using length ~d~%" len)
    (dotimes (j edge)  ; edge effects
             (setf (aref zlo j) 
                   (inner-product (select c (iseq 0 xin))
                                  (select x (iseq xin 0))  ))
             (setf (aref zhi j)
                   (inner-product (select hc (iseq 0 xin))
                                  (select xr (iseq xin 0)) ))
             (incf xin xincr)
             (if (< n xin) (return)))
    (setf xin (iseq xin 0))
    (for (j edge (1- len) 1)
         (setf (aref zlo j)
               (inner-product c (select x xin)))
         (setf (aref zhi j)
               (inner-product hc (select xr xin)))
         (incf xin xincr)
         )
    (list zlo (reverse zhi)) ;  (sqrt 2) built into coef normalization
    ))



#|;------------------------------------------------------------------------

   Wavelet Reconstruction

   ; --- reconstruction by hand, first with Haar basis
   (def wd (wavelet-decomposition  *haar* (setf x '(9 1 2 0  9 1 2 0))))
   (setf r1 (qm-inverse *haar* (first wd) (second wd)))
   (setf r2 (qm-inverse *haar* r1 (select wd 2)))
   (setf r3 (qm-inverse *haar* r2 (select wd 3)))
   (format t "Reconstruction error: ~%~a~%" (- x r3))

   ; --- now with daubechies coefs and periodic coefficients (exact)
   (setf x '(9 1 2 0  9 1 2 0))
   (def wd (fwt x *d4* :periodic? t))
   (setf r1 (periodic-qm-inverse *d4* (first wd) (second wd)))
   (setf r2 (periodic-qm-inverse *d4* r1 (select wd 2)))
   (setf r3 (periodic-qm-inverse *d4* r2 (select wd 3)))
   (format t "Reconstruction error: ~%~{ ~7,4f ~}~%" (- x r3))
   (format t "               error: ~%~{ ~7,4f ~}~%" 
           (- x (ifwt wd *d4* :periodic? t))) 

   ; --- Reconstruction is perfect except at the ends of the data for each
   ;     layer of quad mirror filtering.  Gets worse for wavelet decomp since
   ;     makes its way more and more through data as reach deeper.
   (setf x  (binomial-rand 128 20 .5))
   (setf x  (- x (mean x)))          ; center data and pad
   (setf wd (fwt  (combine (repeat 0 64) x (repeat 0 64))  *d12* :periodic? t))        
   (setf xr (select (ifwt wd *d12* :periodic? t)
                    (+ 64 (iseq 128))))
   (def  p (plot-lines (iseq (length x)) (- x xr)))
   (send p :add-points (iseq 128) x)
   (send p :add-points (iseq 128) xr :color 'red)
   (send p :adjust-to-data)


|#;------------------------------------------------------------------------


(defun iFWT (wd coefs &key periodic?)
  ; Reconstructs series from the coefficients in wd (in format of wavelet-decomp)
  ; 12 May 95 ... 16 May 95
  (let* ((filter (if periodic? #'periodic-qm-inverse #'qm-inverse))
         (x      (funcall filter coefs (first wd) (second wd)))  )
    (dolist (w (cddr wd))  ; drop scaling and biggest wavelet coef
            (setf x (funcall filter coefs x w))
            )
    x))


#|;------------------------------------------------------------------------

   Quadrature Mirror Reconstruction

   (setf qm (qm-filter *haar* (* *haar* '(1 -1)) '(9 1)))
   (qm-inverse *haar* (first qm) (second qm))

   (setf qm (qm-filter *haar* (* *haar* '(1 -1)) '(9 1 2 0   9 1 2 0)))
   (qm-inverse *haar* (first qm) (second qm))

   (setf qm (qm-filter *d4* (* *d4* '(1 -1 1 -1)) '(9 1 2 0)))
   (qm-inverse *d4* (first qm) (second qm))


   ; --- reconstruction is perfect except at the ends of the data
   (setf x   (iseq 8))
   (setf scl *d6*)
   (setf qm (qm-filter scl (wave-from-scale scl) x))
   (setf xr (qm-inverse scl (first qm) (second qm)))
   (def p (plot-points (iseq (length x)) (- x xr)))

   ;     check with matrices
   (def  L (filter-matrix scl :dim (length x)))
   (def  H (filter-matrix scl :dim (length x) :hi? t))
   (def  lo (matmult L x))  (def hi (matmult H x))
   (def  xrr (+ (matmult lo L) (matmult hi H)))
   (format t "Matrix check of qm error: ~%~a~%" (- xr xrr))

   ; --- reconstruction is perfect for periodic boundary wavelet
   (setf x   (iseq 8))
   (setf scl *d6*)
   (setf pqm (periodic-qm-filter scl(wave-from-scale scl) x))
   (setf xr (periodic-qm-inverse scl (first pqm) (second pqm)))
   (def p (plot-points (iseq (length x)) (- x xr)))

   (setf LP  (periodic-filter-matrix *d4* :dim (length x)))
   (setf HP  (periodic-filter-matrix *d4* :dim (length x) :hi? t))
   (+ (matmult (first pqm)  lp)
      (matmult (second pqm) hp))


|#;------------------------------------------------------------------------


(defun PERIODIC-QM-INVERSE (c lo hi)
  ; Reconstruct data from lo and hi orthogonal band-pass qmf filter.
  ; 16 May 95
  (let* ((nc    (length c))
         (hc    (* c (^ -1 (iseq nc))))
         (half  (length lo))
         (loIn  (coerce (iseq 1 (- 2 nc)) 'vector))
         (hiIn  (coerce (iseq nc) 'vector))
         (n     (* 2 half))
         (x     (make-array n :initial-element 0.0))  )
    (dotimes (k half)
             (dotimes (i nc)
                      (incf (aref x (mod (aref loin i) n))
                                  (* (aref c i) (aref lo k))))
             (dotimes (i nc)
                      (incf (aref x (mod (aref hiIn i) n))
                                  (* (aref hc i) (aref hi k))))
             (incf loIn 2)
             (incf hiIn 2)
             )
    x ; (sqrt 2) normalization in coefs
    ))


(defun QM-INVERSE (c lo hi)
  ; Reconstruct data from lo and hi orthogonal band-pass qmf filter.
  ; 12 May 95 ... 16 May 95
  (if (= 1 (length lo))      
      (/ (vector (+ (* (aref lo 0) (aref c 1))
                    (* (aref hi 0) (aref c 0)))
                 (- (* (aref lo 0) (aref c 0))
                    (* (aref hi 0) (aref c 1)))) 
         (sum (^ (select c '(0 1)) 2))  )  ; force coefs to be normed
      (let* ((hi     (reverse hi))             ; reverse hi-pass
             (half   (length lo))
             (n      (* 2 half))               ; length of result
             (n-1    (- n 1))
             (nc     (length c))
             (hc     (* c (^ -1 (iseq nc))))
             (x      (make-array n :initial-element 0.0))
             (lim    0)
             (zin    0)
             (xin    0)   )
        (dotimes (k (min half (- (/ nc 2) 1)))
                 (incf zin 1)
                 (setf lim (* 2 (1+ k)))
                 (dotimes (j (min n lim))
                          (incf (aref x j)
                                (* (aref c  (- lim (1+ j))) (aref lo k))) 
                          (incf (aref x (- n-1 j))
                                (* (aref hc (- lim (1+ j))) (aref hi k)))
                          ))
        (setf xin (iseq nc))
        (setf c (reverse c))
        (setf hc (reverse hc))
        (dotimes (j (- half zin))
                 (incf (select x xin)
                       (* c  (select lo zin)))
                 (incf (select x (- (1- n) xin))
                       (* hc (select hi zin)))
                 (incf zin)
                 (incf xin 2)
                 )
        x ; (sqrt 2) normalization in coefs
        )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun PRINT-ROUND-MAT (mat)
  ; Print matrix as integers
  (print-matrix (round (* 100 mat))))

(defun WAVE-FROM-SCALE (coefs)
  ; Coefs with +/- signs... dont reverse (done in the qm programs)
  (* coefs (^ -1 (iseq (length coefs))))  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro FOR ((var start stop inc) &body body)
  (let ((gstop (gensym))  )
    `(do ((,var ,start (+ ,var ,inc))
          (,gstop ,stop))
         ((> ,var ,gstop))
         ,@body)))
          
; (for (i 2 4 1) (format t "i = ~a~%" i))
; (for (i 6 6 1) (format t "i = ~a~%" i)
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun C-FWT  (data coefs &key (periodic? t))
  (unless periodic? (format t "C only computes periodic.~%"))
  (first (last 
          (call-cfun "WaveletDecomp"
                     (float data) (length data)
                     (float coefs) (length coefs)
                     (repeat 7.7 (length data))))
         ))
            
(defun C-IFWT (wd coefs &key (periodic? t))
  (unless periodic? (format t "C only computes periodic.~%"))
  (first (last
          (call-cfun "WaveletRecon"
                     (float wd) (length wd)
                     (float coefs) (length coefs)
                     (repeat 7.7 (length wd)))
          )))
       
       
(defun C-THRESHOLD-WD (wd threshold type &key (skip 1) sigma)
  ; Apply thresholding to wavelet coefs below threshold, skipping skip levels.
  ; By default, skips the scaling overall coefficient (leading term)
  ; 15 May 95 ... 30 Jul 95 ... 14 Aug 95
  (let* ((bound (* threshold (if sigma sigma (wd-scale wd)))  ))
    ; (format t "~a lim = ~6,2f = ~6,2f x ~6,2f (sigma)~%"
              ;         type bound threshold sigma)
    (first
     (call-cfun "ThresholdWavelet"
                    (float wd) (length wd)
                (round skip) (float bound)
                (if (eq type 'hard) 0 1)))
        ))
       
       
             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
   ; --- check one step of periodic QMF

   (setf x (iseq 16))

   (setf pqm  (periodic-qm-filter *d6* (wave-from-scale *d4*) x))

   ; (print-round-mat 
   (setf LP  (periodic-filter-matrix *d6* :dim (length x)))
   (setf lo  (matmult LP x))
   (format t "Lo ~{ ~8,4f ~}~%" lo)
   (format t "Low difference = ~a~%" (- lo (first pqm)))

   ; (print-round-mat 
   (setf HP  (periodic-filter-matrix *d6* :dim (length x) :hi? t))
   (setf hi  (matmult HP x))
   (format t "Hi ~{ ~8,4f ~}~%" hi)
   (format t "Hi  difference = ~a~%" (- hi (second pqm)))

   ; --- check decomposition for same series

   (round (* 1000 (setf wd (fwt x *d6* :periodic? t))))


   (send *code-manager* :load 'wavelet)

   (def x (rseq 0 100 512))

   (defun lisp-wave (x coefs)
    (let* ((wd  (fwt x coefs :periodic? t))
           (wdt (threshold-wd wd 0 'hard :skip 2))
           )
      (ifwt wdt coefs :periodic? t)
      ))

   (defun c-wave (x coefs)
     (let* ((wd  (c-fwt x coefs :periodic? t))
            (wdt (c-threshold-wd wd 0 'hard :skip 2))  
            )
       (c-ifwt wdt coefs :periodic? t)
       ))

   ; ---  Check the times and compare fits

   (time (def lx (lisp-wave x *d6*)))
   (time (def cx (c-wave    x *d6*)))

   (def xax (iseq (length x)))
   (def p (plot-points (iseq (length x)) x))
   (send p :add-lines xax lx :color 'red)
   (send p :add-lines xax cx :color 'blue)

   (set-working-directory "Dave:Work:Wavelets")
   (compile-file "WaveCalc.lsp")

;                 lisp                compiled lisp?             C

; n = 128/D6      0.70 sec; 0.07    0.62 seconds; 0.02     0.02 sec; 0.00
; n = 512/D6      2.43 sec; 0.12                           0.13 sec; 0.08

   ; --- Draw some wavelets.
   (def n 256)
   
   ;--- 
   (def wd (repeat 0 n))
   (setf (elt wd 10) 2)
   (def recon (c-ifwt wd *d4*))
   (def p4 (plot-lines (rseq 0 1 n) recon))
   (send p4 :range 1 -.75 .5) (send p4 :size 220 200)
   (send p4 :y-axis t t 6)

   ;--- 
   (def wd (repeat 0 n))
   (setf (elt wd 10) 2)
   (def recon (c-ifwt wd *d8*))
   (def p8 (plot-lines (rseq 0 1 n) recon))
   (send p8 :range 1 -.75 .5)  (send p8 :size 220 200)
   (send p8 :y-axis t t 6)

|#

 
  
