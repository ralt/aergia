(in-package #:aergia)

(defvar *default-shell* "/bin/bash")
(defvar *prefix* "common-lisp/")
(defvar *command* "make test")
(defvar *ssh-identity* (concatenate 'string "/home/" (uiop:getenv "USER") "/.ssh/id_rsa"))

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
	  (username (get-arg arguments "username")))
      (unless clone
	(leave "The --clone argument is required."))
      (unless username
	(leave "The --username argument is required."))
      (let* ((clone-name (generate-clone-name clone))
	     (project (first (last (pathname-directory (cat (sb-unix:posix-getcwd) "/")))))
	     (project-remote-path (merge-pathnames project (cat "/home/" username "/" *prefix*))))
	(lxc-clone clone clone-name)
	(lxc-start clone-name)
	(sleep 5) ; give some time to the container to get an IP
	;; How did I know that it was 5 seconds? Practice.
	;; Rewrite this to try getting it every second though
	(let ((ip (lxc-get-ip clone-name)))
	  (lxc-synchronize-project username ip project-remote-path *ssh-identity*)
	  (lxc-run-tests username ip project-remote-path *ssh-identity* *command*)
	  (lxc-cleanup clone))))))

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
