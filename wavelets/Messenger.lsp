;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                       ;;
;;     MESSENGER Dialog                                  ;;
;;                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

   29 Jul 95 ... Cut from AXIS code.

|#

(provide "messenger")

;;;;


(defun MESSENGER-ITEM (target &key dataset methods desc) ; 2/26/94
  (send menu-item-proto :new "Send message" :action
        #'(lambda ()
            (messenger target dataset methods desc))))


;;;;


(defun MESSENGER (target dataset methods desc)
  ; Modified 12 Jul 95 to sort methods.
  (if target
      (send messenger-proto :new
            target dataset
            (if methods methods 
                (sort           
                 (remove-duplicates
                  (send target :method-selectors))
                 #'(lambda (a b) (string< (string a) (string b)))))   
            desc)
      (format t "Unable to build messenger; target is nil.~%")
      ))


;;;;


(defproto MESSENGER-PROTO
  '(target    ; object to be sent the message.
    dataset   ; evaluation context.  If nil, done globally (eval)
    methods   ; symbols to be browsed with scroller
    desc      ; optional description
    formItem  ; form to read for evaluation
   )
  nil
  *object*
  )

(defmeth messenger-proto :ISNEW (target dataset methods desc)
  (setf (slot-value 'target) target)
  (setf (slot-value 'dataset) dataset)
  (setf (slot-value 'methods) methods)
  (setf (slot-value 'desc) desc)
  (send self :open))


(defmeth messenger-proto :BUILD-DIALOG-ITEMS ()
  (let* ((methods (slot-value 'methods))
         (desc    (slot-value 'desc))
         (cLabel  (send text-item-proto :new "Message"))
         (cmd     (send list-form-item-proto :new           ; 7/12/95
                        (strcat ":" (string (first methods)))
                        :text-length 25))
         (dLabel  (send text-item-proto :new "Description:"))
         (dItem   (send text-item-proto :new 
                        (if desc (first desc) "") :text-length 30))
         (helper  #'(lambda (x)
                      (send dItem :text (send self :describe x))))
         (scroll  (send sequence-scroll-item-proto :new
                        (iseq (length methods))
                        :text-item cmd  :display methods
                        :action helper))
         (doit    (send button-item-proto :new "Send It"
                        :action #'(lambda () (send self :evaluate))))  )
    (setf (slot-value 'formItem) cmd)
    (list (list cLabel cmd) 
          (list doit scroll)
          dlabel dItem)
    ))


(defmeth messenger-proto :DESCRIBE (i)
  ; Description for ith method
  (if (slot-value 'desc)
      (select desc i)
      (let ((doc (send (slot-value 'target)
                       :documentation (select (slot-value 'methods) i)))   )
        (if doc doc "None available.")
        )))


(defmeth messenger-proto :EVALUATE ()
  (let* ((res nil)
         (form (send (slot-value 'formItem) :form))
         (cmd `(send ,(slot-value 'target) ,@form))   )
    (format t ">~a~%" (append '(send it) form))
    (setf res
          (if (slot-value 'dataset)
              (send (slot-value 'dataset) :evaluate cmd) 
              (eval cmd)))
    (format t "~a~%" res)
    res ))


(defmeth messenger-proto :OPEN ()
  (let ((dialog (send dialog-proto :new
                      (send self :build-dialog-items)))   )
    (send dialog :title 
          (strcat  (format nil "~a"
                           (send (slot-value 'target)
                                 :slot-value 'proto-name))
                   " Messenger"))
    ))

#||
(def p  (plot-points (uniform-rand 20) (uniform-rand 20)))
(def msg  (list :abline :add-points :add-lines :range :adjust-to-data))
(messenger p nil msg)
||#  


(defproto FORM-ITEM-PROTO
  ()
  ()
  edit-text-item-proto
  ; An edit-text item that also returns Lisp form.
)

(defmeth form-item-proto :FORM () ;    &optional (enclose nil))
  (string-to-form (send self :text)))

; Returns a form which is a list of items.
(defproto LIST-FORM-ITEM-PROTO 
  ()
  ()
  form-item-proto
)

(defmeth list-form-item-proto :FORM ()
  (let ((form (string-to-form    ; avoid stack overflow error 3/25/94
               (call-method edit-text-item-proto :text)))  )
    (cond
      ((symbolp form) (list form))
      ((numberp form) (list form))
      ((endp    form)     nil    )
      ((listp   form) (if (and (symbolp (first form))
                               (fboundp (first form)) )
                          (list form)   ; wrap (log x)
                            form)  )
      (   t     form))))
        

(defmeth list-form-item-proto :TEXT (&optional txt)  ; 3/27/94
  (if txt
      (call-next-method txt)
      (mapcar #'(lambda (x) (format nil "~a" x))
              (send self :form))))

;;;;;;;;;  utilities


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




(defun STRCAT (&rest args)
  (apply #'concatenate 'string args))