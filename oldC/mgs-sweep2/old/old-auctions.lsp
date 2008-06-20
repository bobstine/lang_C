

;;;;
;;;; Code to prototype experts for driving the gsmodel program
;;;;



;;;;   display

(defmeth expert-proto :DISPLAY (&optional window x y)
  ;; Draw in window if available, or print to listener
  (if window
      (send self :draw-in-window window x y)
    (format t "~{~a ~}~%" (send self :status-string))
    ))

(defmeth expert-proto :DRAW-IN-WINDOW (w x y)
  ;; if window, then put at input x positions if given a list of them
  (let* ((str  (send self :status-string))
	 (xPos (if (and (listp x) (= (length x) (length str))) x
		 (accumulate #'+ (cons x (+ 1 (mapcar #'length (butlast str)))))
		 ))  )
    (format t "~a~%" xpos)
    (mapcar #'(lambda (s x) (send w :draw-string s x y))
	    str (* 10 xPos))
    ))

(defmeth expert-proto :STATUS-STRING ()              ; <-------- May override
  (mapcar
   #'(lambda (s x) (format nil s x))
   (list "p=~6,4f" "~a"  "Bal=~,2f" "#hist=~d")
   (list  (first (slot-value 'currentOp))
	  (second (slot-value 'currentOp))
	  (slot-value 'balance) 
	  (length (slot-value 'history)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto EXPERT-WINDOW-PROTO
  '(experts)
  ()
  graph-window-proto)

(defmeth expert-window-proto :ADD-EXPERT (expert)
  (push expert (slot-value 'experts))
  (send self :redraw)
  )

(defmeth expert-window-proto :REDRAW ()
  (call-next-method)
  (let ((x 1)
	(y 0) (dy 10)  )
    (dolist (e (slot-value 'experts))
	    (send e :display self x (incf y dy))  )
    ))



(defproto old-PROTO
  '(    nSinceLastHit
    index ; denotes 'position' in the underlying sequence, 1 based
    )
  ()
  expert-proto)

(defmeth sequence-expert-proto :ISNEW (model balance geoRate geoShare)
  (setf (slot-value 'geoRate)       geoRate)
  (setf (slot-value 'geoShare)      geoShare)
  (setf (slot-value 'index)         (send self :initial-index))
  (setf (slot-value 'nSinceLastHit) 0)
  (call-next-method model balance) ;call last since will init operator
  )

(defmeth sequence-expert-proto :STATUS-STRING ()
  (cons
   (format nil "i=~3d gRate=~4,2f gShr=~4,2f"
	   (slot-value 'index)
	   (slot-value 'geoRate) (slot-value 'geoShare))
   (call-next-method)))
			
(defmeth sequence-expert-proto :NEXT-OPERATOR ()
  (setf (slot-value 'nSinceLastHit)
	(1+ (slot-value 'nSinceLastHit)))
  (setf (slot-value 'index)
	(send self :increment-index (slot-value 'index)))
  (setf (slot-value 'currentOp)
	(list (send self :coding-probability)
	      (send self :operator-for-index (slot-value 'index))
	      )))

(defmeth sequence-expert-proto :OPERATOR-ACCEPTED () ;  <---- May override
  (setf (slot-value 'nSinceLastHit) 0)
  (call-next-method))


;;;;

