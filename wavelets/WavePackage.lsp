;;;;
;;;;  Wavelet Package Definitions
;;;;

(defpackage "WAVELET-PACKAGE" (:nicknames "WAVE")
            (:use "XLISP" "SHARED-LIBRARY"))

(in-package "WAVELET-PACKAGE")


;;;;
;;;; Access to shared libraries
;;;;

(use-package "SHARED-LIBRARY")

;--- Get functions from wavelet shared library
  (setf libPath      "Dave:Documents:RAS C:XLispCR:Wavelets:wavelet.shlb")
  (setf waveLib     (shared-library::shlib-open libPath))
  (setf waveDecomp  (shared-library::shlib-symaddr waveLib "WaveletDecomp"))


;;;;
;;;; Private Functions
;;;;


(defun PAD (x)
  ; Pads to length of form 2^k
  (let* ((n (length x))
         (m (^ 2 (ceiling (log n 2))))  )
    (if (= n m) x
        (progn
         (format t "Padding data to length ~d.~%" m)
         (combine x (repeat 0.0 (- m n))))
        )))

;;;;
;;;; Public Functions
;;;;

(export '(wavelet-decomposition *haar*))

(require "WaveCoef")

(defun WAVELET-DECOMPOSITION (data coef)
  ; Computes periodic wavelet decomposition
  ; Destructive effects on x
  ; 23 Jan 98
  (let* ((x    (coerce (pad data) '(vector c-double)))
         (n    (length x))
         (c    (coerce coef '(vector c-double)))
         (k    (length c))
         (res  (make-array n :element-type 'c-double))   )
    (apply #'shared-library::call-by-address waveDecomp
           (mapcar #'shared-library::array-data-address
                   (list x (coerce (list n) '(vector c-long))
                         c (coerce (list k) '(vector c-long))
                         res))   )
    res))
    



