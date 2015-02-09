(in-package #:aergia)

(defvar *default-shell* "/bin/bash")
(defvar *ignore-file* ".gitignore")
(defvar *prefix* "common-lisp/")
(defvar *command* "make test")

(defun main (args)
  "Application's entry point"
  (multiple-value-bind (commands arguments not-implemented)
      (getopt:getopt args
		     '(("clone" :required) ; the container to clone
		       ("username" :required))) ; the username of the container
    (declare (ignore commands))
    (when not-implemented
      (leave (cat "Not implemented argument(s): " (reduce #'cat-with-spaces
							  not-implemented))))
    (let ((clone (get-arg arguments "clone"))
	  (username (get-arg arguments "username"))
	  (project (first (last (pathname-directory (cat (sb-unix:posix-getcwd) "/"))))))
      (unless clone
	(leave "The --clone argument is required."))
      (unless username
	(leave "The --username argument is required."))
      (let ((clone-name (generate-clone-name clone)))
	(lxc-clone clone clone-name)
	(lxc-start clone-name)
	(sleep 5) ; give some time to the container to get an IP
	;; How did I know that it was 5 seconds? Practice.
	(lxc-run-tests username (lxc-get-ip clone-name) *ignore-file* *prefix* project *command*)
	(lxc-destroy clone-name)))))

(defun cat (&rest args)
  (apply #'concatenate 'string args))

(defun cat-with-spaces (&rest args)
  (format nil "~{~A~^ ~}" args))

(defun generate-clone-name (clone)
  "Generates a name for the container. Template: test-CLONE-TIMESTAMP"
  (cat "test-" clone "-" (write-to-string (get-universal-time))))

(defun leave (message)
  "Quits the application with a message"
  (format t "~A~%" message)
  (sb-ext:exit :code 1))

(defun get-arg (args arg)
  "Finds the argument in the list of arguments"
  (cdr (find-if #'(lambda (pair)
		    (string= (car pair) arg))
		args)))
