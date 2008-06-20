;;;;
;;;;      Line searching algorithms
#|
   15 Feb 95 ... Warning message to logit.
   16 Jun 94 ... Add logit functions.
|#

(provide "search")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                       ;
;;        Transformation utilities                                       ;
;;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun INV-LOGIT (x)
  ; Essentially maps -5 5 into [0,1]
  (/ (1+ (exp (- x)))))

(defun LOGIT (p)
  (log (/ p (- 1 p)))
  )
      
;    (plot-function #'(lambda (x) (logit (inv-logit x))) -5 5)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                       ;
;;        Binary and golden section searches                             ;
;;                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|
 (binary-search-value #'(lambda (x) (^ x 2)) 3  0 7 )
|#

(defmacro WHILE (test &body body)
  `(do ()
       ((not ,test))
       ,@body))

(defun BINARY-SEARCH-VALUE (f z x0 x1 &key (eps .001))
  ; Binary search in [x0,x1] for x such that f(x) = z.  Initial values
  ; must bracket the solution.  That is, find f^{-1}(z).
  ; 12 Oct 96
   (let ((f0 (funcall f x0))
        (f1 (funcall f x1))
        (m  0)
        (fm 0)
        (ep (* 2.001 eps))  )
    (while (< ep (- x1 x0))
           (setf m  (/ (+ x0 x1) 2))
           (setf fm (funcall f m))  
           ; (format t "fm = ~a~%" fm)
           (if (< f0 f1)
               (if (< z fm)
                   (setf x1 m f1 fm)
                   (setf x0 m f0 fm))
               (if (< z fm)
                   (setf x0 m f0 fm)
                   (setf x1 m f1 fm))
               ))     
     (* .5 (+ x0 x1))
    ))
         

(defun BINARY-SEARCH (f x0 x1 &key (eps .1))
;  Binary search on the interval x[0] to x[1] of MAX of f
  (print (list x0 x1))
  (if (< (- x1 x0) (* 2.1 eps))
      (/ (+ x0 x1) 2)
      (let* ((m  (/ (+ x0 x1) 2))
             (y- (funcall f (- m eps)))
             (y+ (funcall f (+ m eps)))   )
        (if (< y- y+)
            (binary-search f (- m eps) x1  :eps eps)
            (binary-search f x0 (+ m eps)  :eps eps))
        )))
         
;  (binary-search #'(lambda (x) (- (^ x 2))) -1 1)

;(defun GS-TERMINATES? (fVals)
;  Terminate if functions satisfy this condition.
;  (> .02 (/ (standard-deviation fVals)
;            (abs (mean fVals))))  )


(defun GS-TERMINATES? (fVals)
;  Terminate if function evaluations (f0,f1,f2,f3) satisfy this condition.
  (< (- (max fVals) (min fVals))   ; diff in log like less than .5
     .5) )  

(defun GOLDEN-SEARCH (f x0 x1 &key (eps .05))
;  Golden section search on (x0,x1) for the MAX of f.
;  assuming that y[0] and y[1] are values of f at endpoints.\
;  Terminates when x diff < eps or function diff less than 1%.
;  Also checks for monotone increasing function.
  ; (format t "Initial interval is: ~6,2f -- ~6,2f~%" x0 x1)
  (let*  ((b  (/ (+ -1 (sqrt 5)) 2))  ; .618
          (s  (- 1 b))
          (x3 x1)
          (x1 (+ x0 (* s (- x3 x0))))
          (x2 (+ x0 (* b (- x3 x0))))
          (f0 (funcall f x0))     ; compute f0,f3 for func test
          (f1 (funcall f x1))    
          (f2 (funcall f x2))
          (f3 (funcall f x3))   )
   (do ((len  100   (- x3 x0)) )
       ((or (< len eps) (gs-terminates? (list f0 f1 f2 f3)))
        (if (< f0 f1 f2 f3)   ; result form
            (progn
             (format t "*GS* Function appears monotone.~%")
             (list (list x3) (list f3))  )
            (list (list x0 x3) (list f0 f3)))  )
       ; (format t "e=~5,3f ---> ~6,3f ~6,3f ~6,3f ~6,3f ~%" eps x0 x1 x2 x3)
       (if (< f1 f2)  ; move right
           (progn
            (setf x0 x1) (setf f0 f1)
            (setf x1 x2) (setf f1 f2)
            (setf x2 (+ (* b x1) (* s x3)))
            (setf f2 (funcall f x2)) )
           (progn
            (setf x3 x2) (setf f3 f2)
            (setf x2 x1) (setf f2 f1)
            (setf x1 (+ (* b x2) (* s x0)))
            (setf f1 (funcall f x1))   )
           ) )))


;  (golden-search #'(lambda (x) (- (^ x 2)))  -1 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  
;;     Newton's Method
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun NEWTON (f1 f2 x0 &key f0 (f1Size .001) )
  ; Find the zero of f1 given its deriv f2 starting from xo.
  ; If objective function f0 given, checks for improvement.
  ; ... 18 Nov 94
  ; (format t "Entering newton at ~a~%" x0)
  (let ((fx0 nil)  (x1 -100000)
        (fx1 (if f0 (funcall f0 x0) -1e10))    )
    (labels ((close? (x)
                     ; (format t "f' at est is ~a~%" (funcall f1 x))
                     (< (abs (funcall f1 x)) f1Size))
            (better? (x)
                     (if f0
                         (progn
                          (setf fx1 (funcall f0 x))
                          (format t "       @ ~a --->  ~a~%"
                                  x fx1)
                          (< fx0 fx1))
                         t))
            (next (x)
                  (let ((d1 (funcall f1 x))
                        (d2 (funcall f2 x))  )
                    (do ((nxt (- x (/ d1 d2)) (- x (/ d1 (* p d2))))
                         (p   2  (* -2 p))  )  ; look around
                        ((better? nxt) nxt))))    )
      (loop (setf x0 (next x0))           
            (if ((close? x0) (return x0))) 
            ; (format t "Newton @ ~a ~%" x0)
            (setf fx0 fx1)
            )
      )))


(defun NEWTON-NEXT-SCALAR (x0 func der)
  ; Computes next iterate for newton from func args.
  (let  ((f  (funcall func x0))
         (fd (funcall der  x0))  )
    (if (= fd 0) ; message then die
        (progn
         (format t "Derivative is zero...~%")
         (format t "     [f = ~a,fd = ~a] @ ~a~%" f fd x0)
         x0)
        (- x0 (/ f fd))    )
    ))


(defun MAKE-NUM-DERIV (f &key (h .005))
  ; Build a function which produces the numerical derivative of f.
  #'(lambda (x)
      (let ((fx+ (funcall f (+ x h)))
            (fx- (funcall f (- x h)))  )
        (/ (- fx+ fx-) (* 2 h))  
        )))


(defun MAKE-NUM-GRADIENT (f k &key (h .002))
  ; As above, but with k indicating number of arguments.
  #'(lambda (x)
      (let ((grad (repeat 0 k))
            (x+   (copy-list x))             ; note the need for copy-list
            (x-   (copy-list x))    )
        (dotimes (i k)
                 (setf (select x+ i) (+ (select x i) h))
                 (setf (select x- i) (- (select x i) h))
                 ; (format t "x+ = ~a, x- = ~a~%" x+ x-)
                 (setf (select grad i)
                       (/ (- (funcall f x+) (funcall f x-))
                          (* 2 h)))
                 (setf (select x+ i) (select x i))
                 (setf (select x- i) (select x i))
                 )
        grad)))

