;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                               ;;
;;                    Constructor Dialog                         ;;
;;                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (wavelet-dialog)

(defun WAVELET-DIALOG ()
  ; Result held also in global *wd* (most recent)
  ; 26 Jul 95
  (let* ((dataLab    (send text-item-proto :new "Data series->"))
         (dataForm   (send form-item-proto :new " " :size '(150 20)))
         (lab1       (send text-item-proto :new "Coef.Family"))
         (lab2       (send text-item-proto :new "Boundary"))
         (textitem   (send text-item-proto :new "" :text-length 6))
         (scrollItem (send scroller-proto :new *d-list*
                           :text-item textItem :size '(100 20)))
         (coefItem   (send choice-proto :new scrollItem 
                           (list (list '*haar*) *d-list* *la-list*)
                           (list "Haar" "Daub" "Least Asym") :value 1))
         (boundItem  (send choice-item-proto :new 
                           (list "None" "Periodic")))
         (buildBut   (send button-item-proto :new "Compute Wavelet"
                           :action 
                           #'(lambda ()
                               (let ((cSym (send coefItem :list-value))
                                     (bc?  (< 0 (send boundItem :value)))  )
                                 (setf *wd*
                                       (send wavelet-proto :new 
                                           (eval (send dataForm :form))
                                           (symbol-value cSym)
                                           (if bc? t nil)
                                           (concatenate
                                            'string "Wavelet using " (string cSym)
                                            " with "
                                            (if bc? " periodic." "no edge."))))
                                 (send *wd* :plot-decomposition)))  ))
         (theDialog (send dialog-proto :new
                          (list (list dataLab dataForm)
                                (list lab1 lab2)
                                (list coefItem boundItem)
                                (list textItem)
                                (list scrollItem) 
                                (list buildBut)   )))       
         )
    (send theDialog :title "Wavelet Dialog")
    (send scrollitem :value 0)
    ))

          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproto scroller-proto
  ()
  ()
  sequence-scroll-item-proto)

(defmeth scroller-proto :use-sequence (seq)
  (setf (slot-value 'XLISP::DISPLAY-SEQUENCE) seq)   ; ??? Huh
  (send self :min-value 0)
  (send self :max-value (1- (length seq)))
  (send self :value 0)
  )

(defproto choice-proto
  '(scroller scrollLists)
  ()
  choice-item-proto)

(defmeth choice-proto :isnew (scroller lists &rest args)
  (setf (slot-value 'scroller) scroller)
  (setf (slot-value 'scrollLists) lists)
  (apply #'call-next-method args))

(defmeth choice-proto :do-action ()
  (send (slot-value 'scroller) :use-sequence
        (select (slot-value 'scrollLists)
                (send self :value)))
  (call-next-method))

(defmeth choice-proto :list-value ()
  (select (select (slot-value 'scrollLists) (send self :value))
          (send (slot-value 'scroller) :value))
  )
           
;;;;

(defproto form-item-proto
  ()
  ()
  edit-text-item-proto
  ; An edit-text item that also returns Lisp form.
)

(defmeth form-item-proto :FORM () ;    &optional (enclose nil))
  (string-to-form (send self :text)))



;;;;;;;;;;;;;;;;;;;;;;;;  Utilities  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun STRING-TO-FORM (str)
  (let ((ct 0))  
    (labels ((read-all (s)                 ; 2/27/94
               (let ((x (read s nil)))
                 (if x 
                     (progn (setf ct (1+ ct)) 
                            (cons x (read-all s)))
                     nil))))
    (when str
          (setf str (string-trim " " str))
          (when (< 0 (length str)) ; some non-blank items
                (unless (and         ; add ()'s around text
                         (eq (char "(" 0) (char str 0))
                         (eq (char ")" 0) (char str (1- (length str))))  )
                        (if (member #\Space (coerce str 'list))   ; 4/2/94
                            (setf str (concatenate 'string "(" str ")"))))
                (with-input-from-string (s str)
                                        (let ((res (read-all s))  ) ;2/27/94
                                          (if (= 1 ct)
                                              (first res)
                                              res)
                                          )))))))

