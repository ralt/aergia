(in-package #:aergia)

(defun main (args)
  "Application's entry point"
  (multiple-value-bind (commands arguments not-implemented)
      (getopt:getopt args
		     '(("clone" :required) ; the container to clone
		       ("username" :required) ; the username of the container
		       ; Overridable variables
		       ("command" :optional)
		       ("default-shell" :optional)
		       ("prefix" :optional)
		       ("ssh-identity" :optional)))
    (declare (ignore commands))
    (multiple-value-bind (clone username clone-name project-remote-path)
	(handle-arguments arguments not-implemented)
      (default-variables-let arguments
	  (*default-shell*
	   *prefix*
	   *command*
	   *ssh-identity*)
	(lxc-clone clone clone-name)
	(lxc-start clone-name)
	(let ((ip (lxc-get-ip clone-name)))
	  (lxc-synchronize-project username ip project-remote-path *ssh-identity*)
	  (lxc-run-tests username ip project-remote-path *ssh-identity* *command*)
	  (lxc-cleanup clone))))))

(defun handle-arguments (arguments not-implemented)
  "Handles the nitty-gritty of the arguments stuff"
  (when not-implemented
    (leave (cat "Unavailable argument"
		(when (> (length not-implemented) 1) "s")
		": " (reduce #'cat-with-spaces
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
      (values clone username clone-name project-remote-path))))

(defun cat (&rest args)
  (apply #'concatenate 'string args))

(defun cat-with-spaces (&rest args)
  (format nil "~{~A~^ ~}" args))

(defun generate-clone-name (clone)
  "Generates a name for the container. Template: test-CLONE-TIMESTAMP"
  (cat "test-" clone "-" (write-to-string (get-universal-time))))

(defun leave (message &key (code 1))
  "Quits the application with a message"
  (say-red (cat message "~%"))
  (sb-ext:exit :code code))

(defun get-arg (args arg)
  "Finds the argument in the list of arguments"
  (cdr (find-if #'(lambda (pair)
		    (string= (car pair) arg))
		args)))

(defun say (message)
  "Outputs a message immediately on the screen"
  (format t message)
  (force-output))

(defun say-green (message)
  "Outputs a green message"
  (say message))

(defun say-red (message)
  "Outputs a red message"
  (say message))

(defun clean-stars (var)
  "Removes the stars from a string"
  (cl-ppcre:regex-replace-all "\\*" var ""))
