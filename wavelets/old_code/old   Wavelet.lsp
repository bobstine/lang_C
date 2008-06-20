#|

                        WAVELET.LSP

  19 May 95 ... Enhance with decimation options.
   4 May 95 ... Lisp code to experiment with wavelet transforms.

|#

(require "WaveDef.lsp")

#|;------------------------------------------------------------------------

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
   (def coefs *d4*) (def per nil)
   (def wd (wavelet-decomposition y coefs :periodic? per))

   ; --- Animated reconstruction plot
   (dynamic-threshold-reconstruction wd coefs p :periodic? per)

   ; --- Plots of decomposition 
   ; (mapcar #'standard-deviation (rest wd)) ; first is special
   (boxplot (cddr wd))
   (plot-decomposition wd)

   ; --- Thresholding
   (boxplot (rest wd))
   (plot-decomposition
    (setf wdt (threshold-decomposition wd .8 :skip 1)))
   
   ; --- Wavelet reconstructions without/with thresholding
   ; (send p :add-points (iseq n) (wavelet-reconstruction coefs wd)
   ;       :symbol 'cross)  ; shows effects of boundaries
   (send p :add-lines (iseq n) (wavelet-reconstruction coefs wdt)
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
         

(defun DYNAMIC-THRESHOLD-RECONSTRUCTION (wd coefs plot &key periodic?)
  ; Shows the reconstructed series as animated with a slider.
  ; 15 May 95
  (let* ((sd   (standard-deviation (select wd (1- (length wd)))))
         (y    (wavelet-reconstruction wd coefs :periodic? periodic?))
         (x    (iseq (length y)))
         (cp   (plot-decomposition wd))
         )
    (send cp :location 0 250)
    (send plot :add-lines x y)
    (interval-slider-dialog
     (list 0.0 (* 10 sd))
     :points 50
     :action #'(lambda (threshold)
                 (send plot :clear-lines)
                 (let ((wdt (threshold-decomposition wd threshold :skip 1))  )
                   (send plot :add-lines x
                         (wavelet-reconstruction wdt coefs  :periodic? periodic?))
                   (send cp :clear-lines)
                   (plot-decomposition wdt :addto cp)
                   ))
     )))




(defun PLOT-DECOMPOSITION (wd &key addTo)
  ; WD is wavelet decomp, starting at most coarse stage.
  ; 15 May 95
  (let* ((wd    (rest wd))  ; drop scaling coef
         (depth (+ 2 (- (length wd))))
         (sd    (* 3 (standard-deviation (combine (rest wd)))))
         (wd    (reverse (rest (/ wd      ; rescale, start with fine coefs
                                  (if (zerop sd) 1 sd)))))
         (color 'black)
         (scl   2)
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
                  (1+ (* scl (iseq (length w))))  (coerce (+ w depth) 'list))
            (setf depth (1+ depth)) 
            (setf scl (* 2 scl))
            )
    (unless addto (send p :adjust-to-data))
    p
    ))



(defun COPY-DECOMPOSITION (wd)
  ; 15 May 95
  (cons (copy-list (first wd))
        (mapcar #'copy-vector (rest wd))))



(defun THRESHOLD-DECOMPOSITION (wd threshold &key (skip 1))
  ; Apply thresholding to wavelet coefs below threshold, skipping skip levels.
  ; By default, skips the scaling overall coefficient (leading term)
  ; Returns a copy rather than in-place.
  ; 15 May 95
  (let ((new (copy-decomposition wd))
        (n   (length wd))   )
    (dolist (w (select new (iseq skip (1- n))))
            (dotimes (j (length w))
                     (when (< (abs (aref w j)) threshold)
                           (setf (aref w j) 0.0))
                     ))
    new))



#|;------------------------------------------------------------------------

   (print-matrix (filter-matrix '(c0 c1 c2 c3)))
   (print-matrix (filter-matrix '(c0 c1) :dim 6))
   (print-matrix (filter-matrix '(c0 c1 c2 c3) :dim 10))
   (print-matrix (filter-matrix '(1 2 3 4) :dim 8 :hi? t))

   (setf L (filter-matrix *d8* :dim 8))
   (setf LP (periodic-filter-matrix *d6* :dim 8))
   (setf H (filter-matrix *d8* :dim 8 :hi? t))
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
         (dim (if dim dim (1- n)))
         (k 0)
         (mat (make-array (list (/ dim 2) dim) :initial-element 
                          (if (numberp (aref coefs 0))
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

   (setf wd  (wavelet-decomposition '(9 1 2 0    9 1 2 0) *haar*))

   (setf nwd (wavelet-decomposition '(9 1 2 0    9 1 2 0) *haar* :decimate? nil))

   (def wd (wavelet-decomposition '(1 3 5 4 3 2 1 2 3 4 5 3 5 3 1 3) *d6* 
                                  :periodic? t))

|#;------------------------------------------------------------------------

(defun WAVELET-DECOMPOSITION (data coefs &key (periodic? t) (decimate? t))
  ; Assuming data are of length which is power of 2, returns list of hi freq
  ; wavelet coefficient pairs from underlying qmf. First special with both.
  ; 12 May 95 ... 25 Jul 95
  (let ((lim    (log (length data) 2))
        (count  0)
        (data   (coerce data 'vector))
        (high   (* coefs (^ -1 (iseq (length coefs)))))
        (filter (if periodic? #'periodic-qm-filter #'qm-filter))  )
    (labels ((wd (x)
                 (incf count)
                 (if (= count lim)
                     (list x)
                     (let ((qmf (funcall 
                                 filter coefs high x :decimate? decimate?))  )
                       (cons (second qmf)    ; just save wavelet coefs
                             (wd (first qmf))  ))))     )
      (reverse (wd data))
      )))
               


#|;------------------------------------------------------------------------

   Quadrature Mirror Filter

   (qm-filter *haar* (* *haar* '(1 -1)) '(9 1 2 0))
   (qm-filter *d6* (* *d6* '(1 -1 1 -1 1 -1)) '(9 1 2 0))
   (qm-filter *d6* (* *d6* '(1 -1 1 -1 1 -1)) '(9 1 2 0 0 0 0 0))

   (qmf (reverse *haar*) (* *haar* '(1 -1)) '(9 1 2 0))
   (qmf (reverse *d6*  ) (* *d6* '(1 -1 1 -1 1 -1)) '(9 1 2 0))

   ; --- check quad-mirror calculations via matrices for D6

   (setf x '(1 3 5 4 3 2 1 2 3 4 5 3 5 3 1 3))
   (setf  qm (qm-filter *d6* (wave-from-scale *d6*) x))
   (setf nqm (qm-filter *d6* (wave-from-scale *d6*) x :decimate? nil))

   (int-mat (setf L  (filter-matrix *d6* :dim (length x))))
   (setf lo  (matmult L x))
   (format t "Low difference = ~a~%" (- lo (first qm)))
   (int-mat (setf H  (filter-matrix *d6* :dim (length x) :hi? t)))
   (setf hi  (matmult H x))
   (format t "Hi  difference = ~a~%" (- hi (second qm)))

   ; --- check periodic form

   (setf x '(1 3 5 4 3 2 1 2 3 4 5 3 5 3 1 3))
   (setf pqm  (periodic-qm-filter *d6* (wave-from-scale *d6*) x))
   (setf pqm2 (periodic-qm-filter *d6* (wave-from-scale *d6*) x :decimate? nil))

   (setf LP  (periodic-filter-matrix *d4* :dim (length x)))
   (setf lo  (matmult LP x))
   (format t "Low difference = ~a~%" (- lo (first pqm)))

   (setf HP  (periodic-filter-matrix *d4* :dim (length x) :hi? t))
   (setf hi  (matmult HP x))
   (format t "Hi  difference = ~a~%" (- hi (second pqm)))


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
    (format t "per-qmf: Using length ~d~%" len)
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
   (def wd (wavelet-decomposition *d4* x :periodic? t))
   (setf r1 (periodic-qm-inverse *d4* (first wd) (second wd)))
   (setf r2 (periodic-qm-inverse *d4* r1 (select wd 2)))
   (setf r3 (periodic-qm-inverse *d4* r2 (select wd 3)))
   (format t "Reconstruction error: ~%~a~%" (- x r3))

   ; --- Reconstruction is perfect except at the ends of the data for each
   ;     layer of quad mirror filtering.  Gets worse for wavelet decomp since
   ;     makes its way more and more through data as reach deeper.
   (setf x  (binomial-rand 128 20 .5))
   (setf x  (- x (mean x)))          ; center data and pad
   (setf wd (wavelet-decomposition
             *d12*
             (combine (repeat 0 64) x (repeat 0 64))
             :periodic? t))        
   (setf xr (select (wavelet-reconstruction *d12* wd :periodic? t)
                    (+ 64 (iseq 128))))
   (def  p (plot-lines (iseq (length x)) (- x xr)))
   (send p :add-points (iseq 128) x)
   (send p :add-points (iseq 128) xr :color 'red)
   (send p :adjust-to-data)


|#;------------------------------------------------------------------------


(defun WAVELET-RECONSTRUCTION (wd coefs &key periodic?)
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

(defun INT-MAT (mat)
  ; Print matrix as integers
  (print-matrix (round (* 100 mat))))

(defun WAVE-FROM-SCALE (coefs)
  ; Reverse coefs with +/- signs 
  (* coefs (^ -1 (iseq (length coefs))))  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro FOR ((var start stop inc) &body body)
  (let ((gstop (gensym))  )
    `(do ((,var ,start (+ ,var ,inc))
          (,gstop ,stop))
         ((> ,var ,gstop))
         ,@body)))
          
; (for (i 2 4 1) (format t "i = ~a~%" i))
; (for (i 6 6 1) (format t "i = ~a~%" i))
 
             
 
  
