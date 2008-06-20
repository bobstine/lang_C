#|
    Overlay Controls

    28 Jul 95 ... Created to build wavelet controls.

|#

(provide "overlay")

#|
;; Examples of overlays

; --- Open a graph window
(def g (send graph-proto :new 2))
(defmeth g :redraw ()    ; is this needed?
  (call-next-method)
  (send self :redraw-overlays)
  )

; --- Generic overlay (leave 0.5 for later use by slider overlay)
(def ov1 (send overlay-button-proto :new
               g 100 50 "Generic  " 0 :dec 2))
(send ov1 :value 'shs)
(send ov1 :value 12345)
(send ov1 :value 0.5)

; --- In place slider uses ov1 for its display
(def ov2 (send slider-overlay-proto :new
               g 120 60 50  (rseq 0 1 37) 0 ov1))


; --- Popup for selection(vis-a-vis a collection of radio buttons)
(def ov3 (send popup-overlay-proto :new
               g 100 100 "Popup " 'one (list 'one 'two 'threeddd)))

; --- Slider using external xlisp slider to set its own value
(def ov4 (send lisp-slider-overlay-proto :new
               g 100 150 "Slider " 5 0 10))

|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              ;;
;;              Graph text overlays                             ;;
;;                                           30 Jul 95          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto text-overlay-proto
  '(x y text)                     ; text (left,top) location in window
  ()
  graph-overlay-proto)

(defmeth text-overlay-proto :ISNEW (x y text)
  (setf (slot-value 'x) x)
  (setf (slot-value 'y) y)
  (setf (slot-value 'text) text)
  )

(defmeth text-overlay-proto :REDRAW ()
  (send (slot-value 'graph) :draw-string
        (slot-value 'text) (slot-value 'x) (slot-value 'y)
        ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              ;;
;;              Graph overlay buttons                           ;;
;;                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defproto overlay-button-proto
  '(x y text                     ; text (left,top) location in window
    clickX                       ; left for click region
    textWidth textHeight         ; pixel size for click region
    action                       ; func(ov,x) to call when :value is set 
    value               
    slider                       ; slider control
    wid dec)                     ; format for value box, as in ~wid,decf
  ()
  graph-overlay-proto)


(defmeth overlay-button-proto :ISNEW (window x y text value 
                                             &key (action nil) (wid 5) (dec 1))
  (setf (slot-value 'x)          x)
  (setf (slot-value 'y)          y)
  (setf (slot-value 'text)       text)
  (setf (slot-value 'clickX)     (+ x 2 (send window :text-width text)))
  (setf (slot-value 'textWidth)  (* wid (send window :text-width "0")))
  (setf (slot-value 'textHeight) (+ (send window :text-ascent)
                                    (send window :text-descent)))
  (setf (slot-value 'wid)        wid)
  (setf (slot-value 'dec)        dec)
  (setf (slot-value 'action)     action)
  (setf (slot-value 'value)      value)
  (send window :add-overlay self)
  (send window :redraw-overlays)
  )

(defmeth overlay-button-proto :SLIDER (&optional (slider nil set))
  (if set
      (setf (slot-value 'slider) slider)
      (slot-value 'slider)))

(defmeth overlay-button-proto :REDRAW ()
  (let ((win (slot-value 'graph))
        (wid (slot-value 'wid))
        (v   (slot-value 'value))
        (x   (slot-value 'clickX))
        (y   (slot-value 'y))  )
    (flet ((rect (x y w h)
                 (send win :erase-rect x y w h)
                 (send win :frame-rect x y w h))  )
      (send win :draw-string (slot-value 'text) (slot-value 'x) y)
      (rect (- x 2)
            (- y (slot-value 'textHeight))
            (+ 3 (slot-value 'textWidth)) 
            (+ 3 (slot-value 'textHeight))  )
      (send win :draw-string 
            (cond 
              ((integerp v) (format nil "~vd"  wid v))
              ((numberp v)  (format nil "~v,vf" wid (slot-value 'dec) v))
              ( t           (format nil "~va" wid (string-downcase (string v))))
              )
            x y)
      )))


(defmeth overlay-button-proto :DO-CLICK (x y s o)
  (let ((left    (slot-value 'clickX))
        (bottom  (slot-value 'y))  )
    (when (and (< left x (+ left (slot-value 'textWidth)))
               (< (- bottom (slot-value 'textHeight)) y bottom)  )
          (send self :handle-click x y s o)
          )))

(defmeth overlay-button-proto :HANDLE-CLICK (x y s o) 
  (let ((str (get-string-dialog 
              (format nil "Enter ~s: " (slot-value 'text))))  )
    (if str
        (send self :value str))))

(defmeth overlay-button-proto :VALUE (&optional (x nil set) fromSlider)
  (if set
      (progn (setf (slot-value 'value) x)
             (send self :redraw) 
             (unless fromSlider
                     (if (slot-value 'slider) 
                         (send (slot-value 'slider) :value
                              (if (numberp x) x
                                  (with-input-from-string (s x)
                                                          (read s))))))
             (when (slot-value 'action)
                   (funcall (slot-value 'action) self x))  )
      (slot-value 'value)
      ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              ;;
;;                   Popup overlay                              ;;
;;                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto POPUP-OVERLAY-PROTO
  '(menu)
  ()
  overlay-button-proto)

             
(defmeth popup-overlay-proto :ISNEW (window x y text value popList &key (action nil))
  (call-next-method window x y text value
                    :action action
                    :wid (max (mapcar #'(lambda (x) (length (string x)))
                                      popList))  )
  (send self :build-menu popList)
  )

(defmeth popup-overlay-proto :HANDLE-CLICK (x y s o)            ; OVERRIDE
  (send (slot-value 'menu) :popup
        (slot-value 'clickx) (slot-value 'y)
        (slot-value 'graph))
  )

(defmeth popup-overlay-proto :BUILD-MENU (items)
  (let ((theMenu (send menu-proto :new "Popup"))  )
    (apply #'send theMenu :append-items
          (mapcar #'(lambda (m)
                      (send menu-item-proto :new (string m)
                            :action #'(lambda () (send self :value m))))
                  items))
    (setf (slot-value 'menu) theMenu)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              ;;
;;                Lisp Slider overlay                           ;;
;;                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defproto LISP-SLIDER-OVERLAY-PROTO
  '(min max)
  ()
  overlay-button-proto)

             
(defmeth lisp-slider-overlay-proto :ISNEW (window x y text value min max
                                            &key (wid 6) (dec 2))
  (call-next-method window x y text value
                    :wid wid :dec dec)
  (setf (slot-value 'min) min)
  (setf (slot-value 'max) max)
  )

(defmeth lisp-slider-overlay-proto :HANDLE-CLICK (x y s o)          ; OVERRIDE
  (interval-slider-dialog
   (list (slot-value 'min) (slot-value 'max))
   :points 50 
   :action #'(lambda (x) (send self :value x))
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              ;;
;;                    Slider overlay                            ;;
;;                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defproto SLIDER-OVERLAY-PROTO
  '(x y  position          ; location of left end in graph, current x position
    width height           ; height is size of box
    seq index len          ; sequence and current index
    min max                ; computed from input sequence
    textOverlay            ; overlay holding value
    )
  ()
  graph-overlay-proto)


(defmeth slider-overlay-proto :ISNEW (window x y length seq index textOverlay)
  (setf (slot-value 'x)           x)
  (setf (slot-value 'y)           y)
  (setf (slot-value 'height)      4)
  (setf (slot-value 'width)       length)
  (setf (slot-value 'seq)         seq)
  (setf (slot-value 'len)         (length seq))
  (setf (slot-value 'index)       index)
  (setf (slot-value 'textOverlay) textOverlay)
  (setf (slot-value 'min)         (first seq))
  (setf (slot-value 'max)         (first (last seq)))
  (send self :calc-position)
  (send window :add-overlay self)
  (send textOverlay :slider self)
  (send window :redraw-overlays)
  )


(defmeth slider-overlay-proto :VALUE (value)
  (setf (slot-value 'index)
        (cond
          ((< value (slot-value 'min)) 0)
          ((> value (slot-value 'max)) (1- (slot-value 'len)))
          ( t  (let ((dev (abs (- value (slot-value 'seq))))  )
                 (position (min dev) dev)))
          ))
  (send self :calc-position)
  (send self :redraw))


(defmeth slider-overlay-proto :CALC-POSITION()
  ; Left edge of the box
  (setf (slot-value 'position)
        (+ (slot-value 'x)
           (floor
            (* (slot-value 'width) (/ (slot-value 'index)
                                      (slot-value 'len)))
            ))))

        
(defmeth slider-overlay-proto :REDRAW ()
  (let ((win (slot-value 'graph))
        (h   (slot-value 'height))
        (w   (slot-value 'width))
        (x   (slot-value 'x))
        (y   (slot-value 'y))  )
    (send win :erase-rect x (- y h) (+ 3 w) (+ y h))
    (send win :draw-line x y (+ x w) y)
    (send win :frame-rect (slot-value 'position) (- y h) h (+ h h))
    ))
 
(defmeth slider-overlay-proto :DO-CLICK (x y s o)
  (let ((pos   (slot-value 'position))
        (left    (slot-value 'x))
        (v       (slot-value 'y))  )
    (when (and (< (- v 4) y (+ v 4))
               (< left x (+ left (slot-value 'width))))
          (if (< pos x (+ pos (slot-value 'height)))
              (send self :handle-click x y s o)
              (send self (if (< x pos) :move-left :move-right))
              ))))
    
(defmeth slider-overlay-proto :MOVE-LEFT ()
  (when (< 0 (slot-value 'index))
         (decf (slot-value 'index))
         (send self :update)
         (send (slot-value 'textOverlay) :value 
               (select (slot-value 'seq) (slot-value 'index)) t)
         ))

(defmeth slider-overlay-proto :MOVE-RIGHT ()
   (when (> (1- (slot-value 'len)) (slot-value 'index))
         (incf (slot-value 'index))
         (send self :update)
          (send (slot-value 'textOverlay) :value 
               (select (slot-value 'seq) (slot-value 'index)) t)
         ))
 


(defmeth slider-overlay-proto :MOVE-TO (pct)
  (let ((index (floor (* (1- (slot-value 'len)) pct))))
    (send (slot-value 'textOverlay) :value
          (select (slot-value 'seq) index) t)
    (setf (slot-value 'index) index)
    (send self :update)))
  

(defmeth slider-overlay-proto :HANDLE-CLICK (x y s o) 
  (let* ((win  (slot-value 'graph))
         (wid  (slot-value 'width))
         (left (slot-value 'x))
         (right (+ left wid))
         (oldX  (slot-value 'position))  )
    (send (slot-value 'graph) :while-button-down 
          #'(lambda (x y)
              (when (<= left x right)
                    (send self :move-to (/ (- x left) wid)))))
    ))

       
(defmeth slider-overlay-proto :UPDATE ()
  (let ((win (slot-value 'graph))
        (h   (slot-value 'height))
        (w   (slot-value 'width))
        (x   (slot-value 'x))
        (y   (slot-value 'y))  )
    (send win :draw-mode 'xor)
    (send win :frame-rect (slot-value 'position) (- y h) h (+ h h))
    (send win :draw-mode 'normal)
    (send win :draw-line x y (+ x w) y)
    (send win :frame-rect (send self :calc-position) (- y h) h (+ h h))
    ))

