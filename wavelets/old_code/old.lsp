;;;;;
;;;;;      Old Wavelet code archive
;;;;;


            
  
  


(defun  QM-INVERSE (coefs lo hi)
  ; Reconstruct data from lo and hi orthogonal band-pass qmf filter.
  ; 12 May 95
  (if (= 1 (length lo))      
      (/ (vector (+ (* (aref lo 0) (aref coefs 1))
                    (* (aref hi 0) (aref coefs 0)))
                 (- (* (aref lo 0) (aref coefs 0))
                    (* (aref hi 0) (aref coefs 1)))) 
         (sum (^ (select coefs '(0 1)) 2))  )  ; force coefs to be normed
      (let* ((hi     (reverse hi))             ; reverse hi-pass
             (half   (length lo))
             (n      (* 2 half))               ; length of result
             (c      (coerce coefs 'vector))   ; coefs as vectors
             (nc     (length coefs))
             (hc     (coerce (* coefs (^ -1 (iseq nc))) 'vector))
             (x      (make-array n :initial-element 0.0)) 
             (zin    0)
             (xin    0)   )
        (when (< 2 nc)
              (incf zin 1)
              (setf (aref x 0)       (* (aref c 1) (aref lo 0))) 
              (setf (aref x 1)       (* (aref c 0) (aref lo 0))) 
              (setf (aref x (- n 1)) (* (aref hc 1) (aref hi 0))) 
              (setf (aref x (- n 2)) (* (aref hc 0) (aref hi 0)))   )
        (when (< 4 nc)
              (incf zin 1)
              (incf (aref x 0)       (* (aref c 3) (aref lo 1))) 
              (incf (aref x 1)       (* (aref c 2) (aref lo 1))) 
              (setf (aref x 2)       (* (aref c 1) (aref lo 1))) 
              (setf (aref x 3)       (* (aref c 0) (aref lo 1))) 
              (incf (aref x (- n 1)) (* (aref hc 3) (aref hi 1))) 
              (incf (aref x (- n 2)) (* (aref hc 2) (aref hi 1))) 
              (setf (aref x (- n 3)) (* (aref hc 1) (aref hi 1))) 
              (setf (aref x (- n 4)) (* (aref hc 0) (aref hi 1)))   )
        (when (< 6 nc)
              (incf zin 1)
              (incf (aref x 0)       (* (aref c 5) (aref lo 2))) 
              (incf (aref x 1)       (* (aref c 4) (aref lo 2))) 
              (incf (aref x 2)       (* (aref c 3) (aref lo 2))) 
              (incf (aref x 3)       (* (aref c 2) (aref lo 2))) 
              (setf (aref x 4)       (* (aref c 1) (aref lo 2))) 
              (setf (aref x 5)       (* (aref c 0) (aref lo 2))) 
              (incf (aref x (- n 1)) (* (aref hc 5) (aref hi 2))) 
              (incf (aref x (- n 2)) (* (aref hc 4) (aref hi 2))) 
              (incf (aref x (- n 3)) (* (aref hc 3) (aref hi 2))) 
              (incf (aref x (- n 4)) (* (aref hc 2) (aref hi 2))) 
              (setf (aref x (- n 5)) (* (aref hc 1) (aref hi 2))) 
              (setf (aref x (- n 6)) (* (aref hc 0) (aref hi 2)))   )
        (setf xin (iseq nc))
        (setf c (reverse c))                ; reverse coefs
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
  
  
(defun  QM-FILTER (coefs data)
  ; Compute qmf band-pass filter using coefs applied to data series.
  ; 11 May 95 ... 12 May 95
  (let* ((n      (length data))
         (half   (floor (/ n 2)))
         (c      (coerce coefs 'vector))     ; coefs as vectors
         (nc     (length coefs))
         (hc     (coerce (* coefs (^ -1 (iseq nc))) 'vector))
         (x      (coerce data 'vector))      ; data series as vectors
         (xr     (coerce (reverse data) 'vector))
         (xin    (iseq (1- nc) 0))           ; reverse direction index vec
         (zlo    (make-array half :initial-element 0.0))
         (zhi    (make-array half :initial-element 0.0))
         (zin    0))
    (when (< 2 nc)
          (setf (aref zlo zin) (+ (* (aref c 0) (aref x 1))
                                  (* (aref c 1) (aref x 0))  ))
          (setf (aref zhi zin) (+ (* (aref hc 0) (aref xr 1))
                                  (* (aref hc 1) (aref xr 0))  ))
          (incf zin))
    (when (< 4 nc)
          (setf (aref zlo zin) (+ (* (aref c 0) (aref x 3))
                                  (* (aref c 1) (aref x 2))
                                  (* (aref c 2) (aref x 1))
                                  (* (aref c 3) (aref x 0))  ))
          (setf (aref zhi zin) (+ (* (aref hc 0) (aref xr 3))
                                  (* (aref hc 1) (aref xr 2))
                                  (* (aref hc 2) (aref xr 1))
                                  (* (aref hc 3) (aref xr 0))  ))
          (incf zin))
    (when (< 6 nc)
          (setf (aref zlo zin) (+ (* (aref c 0) (aref x 5))
                                (* (aref c 1) (aref x 4))
                                (* (aref c 2) (aref x 3))
                                (* (aref c 3) (aref x 2))
                                (* (aref c 4) (aref x 1))
                                (* (aref c 5) (aref x 0))   ))
          (setf (aref zhi zin) (+ (* (aref hc 0) (aref xr 5))
                                (* (aref hc 1) (aref xr 4))
                                (* (aref hc 2) (aref xr 3))
                                (* (aref hc 3) (aref xr 2))
                                (* (aref hc 4) (aref xr 1))
                                (* (aref hc 5) (aref xr 0))   ))
          (incf zin))
    (dotimes (j (- half zin))
             (setf (aref zlo zin) (inner-product c (select x (+ (* 2 j) xin))))
             (setf (aref zhi zin) (inner-product hc (select xr (+ (* 2 j) xin))))
             (incf zin)
             )
    (list zlo (reverse zhi)) ;  (sqrt 2) built into coef normalization
    ))
             
