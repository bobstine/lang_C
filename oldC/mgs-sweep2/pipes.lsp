(provide "pipes")

;; --- These file names must agree with those in the C code 
(defconstant outputFileName "driver.input")
(defconstant inputFileName  "driver.output")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;               Pipe utilities
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun MAKE-PIPES ()
  (set-working-directory "/home/bob/C/mgs-sweep2")
  (system (concatenate 'string "rm "
		       inputFileName "  " outputFileName))
  (system (concatenate 'string "mkfifo " inputFileName))
  (system (concatenate 'string "mkfifo " outputFileName))
  t
  )

;;was [ cat driver.input| tee to.spy |driver| tee from.spy > driver.output & ] 

(defun START-EXTERNAL-PROCESS (executableName)
  ; connect the pipes with optional monitor files
  (system "rm log.file")
  (setf *cmdstr* (concatenate 'string executableName " > log.file &"))
  (format t "[ ~a ] ~%" *cmdstr*)
  (system *cmdstr*)
  (system "cat monitor.file >> monitor.archive")
  (setf *monitor* (open "monitor.file" :direction :output))
  (multiple-value-bind (sec min hr date month year)
		       (get-decoded-time)
		       (format *monitor* "Monitoring at ~d/~d/~d  ~d:~d:~d~%"
			       month date year hr min sec))
  (setf *output* (open outputFileName :direction :output))
  (setf *input*  (open inputFileName  :direction :input ))
  )

;; Note that you can check the status of the external command by
;; looking at the status of file variables like *monitor*

(defun RUN-EXTERNAL-COMMAND (str)
  (format t "~%Running command:  ~a~%" str)
  (format *monitor* "==>~a~%" str)
  (force-output *monitor*)
  (format *output* "~a~%" str)
  (force-output *output*)
  (let ((result (read *input*))  )
    (format *monitor* "   ~a~%" result)
    (format t "         result:  ~a~%~%" result)
    result))

(defun END-EXTERNAL-PROCESS ()
  (if *monitor* (close *monitor*))
  (if *output*  (close *output*))
  (if *input*   (close *input*)) )

