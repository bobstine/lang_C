#|
       WaveObj.lsp
 

    25 Jul 95 ... Created to hold definitions of wavelet objects.

|#

(require "Overlay")
(require "WaveCalc")
(require "Messenger")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                               ;;
;;                    Sample of Commands                         ;;
;;                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

 ; --- To run C version you need run these and make sure to use
 ;        c-fwt, c-ifwt, c-threshold-wd
  (load "coderes")
  (send *code-manager* :load 'wavelet)
  


 ; --- 
  (def x (iseq 1 8))

  (def y (sin (* 2 pi (/ (iseq 16) 80))))
  (wavelet-plot y)

|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                               ;;
;;              View of Wavelet Object                           ;;
;;                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro WAVELET-PLOT (dataSym)
  (let* ((data (symbol-value dataSym))
         (n    (length data))
         (x    (iseq n))
         (threshold  (sqrt (* 2 (log n))))
         (plot (plot-points x data))
         (wd   (send wavelet-proto :new data '*haar* :periodic? t :origin 0
                     :threshold threshold :threshold-type 'soft))
         )
    (send plot :size 300 350)
    (send plot :margin 0 80 0 0) ; left top right bottom
    (add-basic-overlays 10 plot wd dataSym n)
    (add-coef-overlays  90 plot wd)
    (add-threshold-overlays 180 plot wd threshold)
    (add-menu-items plot wd)
    (send wd :add-reconstruction-to-plot plot)
    (setf *wd* wd)                             ; global hook
    ))

(defun ADD-MENU-ITEMS (plot wave)
  (let* ((data (send menu-item-proto :new "Hide data"
                     :action #'(lambda ()
                                 (let ((m (send self :mark)))
                                   (if m
                                       (let ((x (send wave :slot-value 'data)))
                                         (send plot :add-points
                                               (iseq (length x)) x)  )
                                       (send plot :clear-points))
                                   (send self :mark (not m))
                                   ))))
         (coefs (send menu-item-proto :new "Plot wavelet coefs"
                      :action #'(lambda ()
                                  (send wave :plot-decomposition))))
         (freez (send menu-item-proto :new "Freeze coef plot"
                      :action #'(lambda ()
                                  (let ((p (send wave :coef-plot)) )
                                    (send wave :coef-plot nil)
                                    (send p :title "Frozen")
                                    (send p :margin 0 20 0 0)
                                    (send p :add-overlay
                                          (send text-overlay-proto :new
                                                25 10 
                                                (format nil "~a ~a coefs (~a @ ~5,2f)"
                                                        (if (send wave :periodic?)
                                                            "Periodic" "Non-periodic")
                                                        (send wave :slot-value 'coefSym)
                                                        (send wave :threshold-type)
                                                        (send wave :threshold))) )
                                    (send p :redraw)
                                    ))))
         )
    (defmeth freez :update ()
      (send self :enabled (if (send wave :coef-plot) t nil)))
    (send (send plot :menu) :append-items
          (send dash-item-proto :new)
          data coefs freez
          (messenger-item plot))
    ))

                    

(defun ADD-BASIC-OVERLAYS (xax plot wd dataSym n)
  ; Added origin slider  30 Jul 95
  (send overlay-button-proto :new plot xax 15 
        " Data  " (format nil "~10a" dataSym)
        :action #'(lambda (o n)
                    (with-input-from-string (s n)
                         (send wd :data (eval (read s)))))
        )
  (send slider-overlay-proto :new plot (+ 3 xax) 50 60 (iseq 0 (1- n)) 0 
        (send overlay-button-proto :new plot xax 40 "Origin " 0
              :action #'(lambda (o x)
                          (send wd :origin 
                                (if (numberp x) x
                                    (with-input-from-string (s x)
                                                            (read s)))))))                              
  )
        
(defun ADD-COEF-OVERLAYS (xax plot wd)
  ; Move to symbol transfer... 30 Jul 95
  (let ((memOv (send popup-overlay-proto :new plot xax 65    ; ??? send coefs
                     "Member " '*Haar* (list '*haar*) :wid 7
                     :action #'(lambda (o x)
                                 (send wd :coefs x))  )))
    (flet ((doit (list)
                 (send memOv :build-menu list)
                 (send memOv :value (first list)))  )
      (send popup-overlay-proto :new plot xax 15 "Bndry " "Periodic"
            (list 'Periodic  'None)
            :action #'(lambda (o x)
                        (case x
                          ('periodic (send wd :periodic? t))
                          ('none     (send wd :periodic? nil))  )))
      (send popup-overlay-proto :new plot xax 40 " Family " 'Haar
            (list 'Haar 'Daub 'LA)
            :action #'(lambda (o x)
                        (case x
                          ('haar  (doit (list '*haar*)))
                          ('daub  (doit  *d-list*))
                          ('la    (doit  *la-list*))  )
                        ))
      )))

(defun ADD-THRESHOLD-OVERLAYS (xax plot wd threshold)
  (let* ((tOv  (send popup-overlay-proto :new plot xax 15 "Threshold " 'soft
                     (list 'None 'Soft 'Hard)
                     :action #'(lambda (o x)
                                 (send wd :threshold-type x))))
         (tOv1 (send overlay-button-proto :new plot xax 40 "  Level  " threshold
                     :wid 6 :dec 2 :action
                     #'(lambda (o x)
                         (send wd :threshold 
                               (if (numberp x) x
                                   (with-input-from-string (s x)
                                                           (read s)))))  ))
         (sl (send slider-overlay-proto :new plot (+ 5 xax) 50 75 
                   (rseq 0 (* 4 threshold) 76) 0 tOv1)))
    (send sl :value threshold)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                               ;;
;;                      Wavelet Object                           ;;
;;                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
; --- build a wavelet object from a short data series
(def x '(1 2 3 4 5 6 7 8))
(def wd (send wavelet-proto :new  x '*D6* :periodic? t :origin 0
              :threshold 0))

; --- plot/print the coefficents
(send wd :plot-decomposition)
(send wd :display)

; --- reconstruct the series
(send wd :reconstruct)

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto WAVELET-PROTO
  '(data coefs coefSym wd sigma              ; decomp and sigma term
    periodic? origin threshold thresholdType ; options
    dataPlot coefPlot                        ; redraws when recomputed
  ))

(defmeth wavelet-proto :ISNEW (data coefSym
                                    &key (periodic? t) (origin 0)
                                    (threshold 0) (thresholdType 'soft))
  (setf (slot-value 'data) (pad-data data))
  (setf (slot-value 'coefs)         (symbol-value coefSym))
  (setf (slot-value 'coefSym)       coefSym)
  (setf (slot-value 'origin)        origin)
  (setf (slot-value 'periodic?)     periodic?)
  (setf (slot-value 'threshold)     threshold)
  (setf (slot-value 'thresholdType) thresholdType)
  (send self :decompose)
  )


;;;;  Slots and accessors  ;;;;


(defmeth wavelet-proto :DECOMPOSITION ()
  (let ((wd (slot-value 'wd))
        (type (slot-value 'thresholdType))  )
    (if (eq 'none type)
        wd
        (threshold-wd wd (slot-value 'threshold) type
                      :sigma (send self :sigma) :skip 1))
    ))

(defmeth wavelet-proto :COEF-PLOT (&optional (p nil set))
  (if set
      (setf (slot-value 'coefPlot) p)
      (slot-value 'coefPlot)))

(defmeth wavelet-proto :SIGMA ()
  (if (slot-value 'sigma)
      (slot-value 'sigma)
      (setf (slot-value 'sigma) (wd-scale (slot-value 'wd)))
      ))


(defmeth wavelet-proto :SET-SLOT-AND-COMPUTE (slot value)
  ; Controls what needs to be done when the value of slot is changed.
  ; Unless change the decomp, only partial updated is needed.
  (setf (slot-value slot) value)
  (unless (member slot '(threshold thresholdType))  ; complete update
          (setf (slot-value 'sigma) nil)
          (send self :decompose))
  (when (slot-value 'dataPlot)
        (send self :add-reconstruction-to-plot (slot-value 'dataPlot) t))
  (when (slot-value 'coefPlot)
        (send self :plot-decomposition))
  )

(defmeth wavelet-proto :DATA (&optional (x nil set))
  (if set
      (let ((dp (slot-value 'dataPlot)) )
        (when dp 
              (send dp :clear-lines) (send dp :clear-points)
              (send dp :add-points (iseq (length x)) x)
              (send dp :adjust-to-data)
              (send self :set-slot-and-compute 'data x)))
      (slot-value 'data))
  )

(defmeth wavelet-proto :COEFS (&optional (x nil set))
  ; Interpreted as a symbol input
  (if set
      (progn (setf (slot-value 'coefSym) x)
             (send self :set-slot-and-compute 'coefs (symbol-value x)))
      (slot-value 'coefs))
  )

(defmeth wavelet-proto :PERIODIC? (&optional (x nil set))
  (if set
      (send self :set-slot-and-compute 'periodic? x)
      (slot-value 'periodic?))
  )

(defmeth wavelet-proto :ORIGIN (&optional (x nil set))
  (if set
      (send self :set-slot-and-compute 'origin x)
      (slot-value 'origin))
  )

(defmeth wavelet-proto :THRESHOLD (&optional (x nil set))
  (if set
      (send self :set-slot-and-compute 'threshold x)
      (slot-value 'threshold))
  )

(defmeth wavelet-proto :THRESHOLD-TYPE (&optional (x nil set))
  (if set
      (send self :set-slot-and-compute 'thresholdType x)
      (slot-value 'thresholdType))
  )



;;;;  Main decompositiona and reconstruction code  ;;;;

(defmeth wavelet-proto :DECOMPOSE ()
  ; Fills the wd slot.
  (setf (slot-value 'wd)
        (fwt (if (zerop (slot-value 'origin))
                 (slot-value 'data)
                 (rotate (slot-value 'data) (slot-value 'origin)))
             (slot-value 'coefs)
             :periodic? (slot-value 'periodic?)
             )))
   
(defmeth wavelet-proto :RECONSTRUCT ()
  (let* ((x (ifwt  (send self :decomposition) (slot-value 'coefs)
                   :periodic? (slot-value 'periodic?))))
    (if (zerop (slot-value 'origin))
        x
        (rotate x (- (length x) (slot-value 'origin)))
        )))


;;;;  Plots and printed listing  ;;;;

(defmeth wavelet-proto :PLOT-DECOMPOSITION ()
  (let ((wd    (send self :decomposition))
        (plot  (slot-value 'coefPlot))   )
    (if plot
        (progn (send plot :clear-lines)
               (plot-wd wd :addTo plot))
        (let ((plot (plot-wd wd))
              (obj  self)  )
          (setf (slot-value 'coefPlot) plot)
          (defmeth plot :close ()  ; make sure it clears the slot
            (send obj :coef-plot nil)
            (call-next-method))  
          plot)
        )))

(defmeth wavelet-proto :PLOT-RECONSTRUCTION (&optional addTo)
  (if addTo
      (setf addTo (plot-points (iseq (length (slot-value 'data)))
                               (slot-value 'data))))
  )


(defmeth wavelet-proto :ADD-RECONSTRUCTION-TO-PLOT (plot &optional clear)
  (setf (slot-value 'dataPlot) plot)
  (if clear (send plot :clear-lines))
  (send plot :add-lines (iseq (length (slot-value 'data)))
        (send self :reconstruct))
  )

(defmeth wavelet-proto :DISPLAY ()
  (print-wd (slot-value 'wd))
  )
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun PAD-DATA (x)
  ; Extends the input list with mean to make length a power of 2.
  (let* ((n   (length x)) 
         (err (- (^ 2 (ceiling (log n 2))) n))  )
    (unless (zerop err)
            (format t "Padding series with ~d mean terms.~%" err)) 
    (if (zerop err)
        x
        (nconc (copy-list x) (repeat (mean x) err))
        )))

(defun ROTATE (x k)
  ; item in location k -> 0, k+1 -> 1, etc
  (let ((n (length x))  )
    (concatenate 'list (select x (iseq k (1- n)))
                  (select x (iseq k)))))


   
  
            