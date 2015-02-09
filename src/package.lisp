(defpackage #:aergia
  (:use #:cl)
  (:export :main))

;;;; Define macros here because this file is the first compiled one
(in-package #:aergia)

(defmacro run (stream &body command)
  "Runs a command. To avoid having an awkward API
 (i.e. passing a list), defining this as a macro."
  `(external-program:run
     (car (list ,@command))
     (cdr (list ,@command))
     :output ,stream
     ;; see man environ
     :environment (list (cons "SHELL" *default-shell*))))

(defmacro clean-run (error-message &body command)
  "Cleanly runs a command. Handles the error."
  (let ((out (gensym))
	(_ (gensym))
	(code (gensym)))
    `(with-output-to-string (,out)
       (multiple-value-bind (,_ ,code)
	   (run ,out ,@command)
	 (declare (ignore ,_))
	 (unless (= ,code 0)
	   (progn
	     (format *error-output* "~A~%" (get-output-stream-string ,out))
	     (leave ,error-message))))
       ,out)))
