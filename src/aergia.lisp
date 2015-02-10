(in-package #:aergia)

(defun main (args)
  "Application's entry point"
  (multiple-value-bind (commands arguments not-implemented)
      (getopt:getopt args
		     '(("clone" :required) ; the container to clone
		       ("username" :required) ; the username of the container
		       ("help" :none)
		       ("version" :none)
		       ; Overridable variables
		       ("command" :optional)
		       ("default-shell" :optional)
		       ("prefix" :optional)
		       ("ssh-identity" :optional)))
    (declare (ignore commands))
    (default-variables-let arguments
	(*default-shell*
	 *prefix*
	 *command*
	 *ssh-identity*)
      (multiple-value-bind (clone username clone-name project-remote-path)
	  (handle-arguments arguments not-implemented)
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
  (when (has-none-arg arguments "version")
    (let ((cl-ansi-text:*enabled* nil))
      (leave (version) :code 0)))
  (when (has-none-arg arguments "help")
    (let ((cl-ansi-text:*enabled* nil))
      (leave (help) :code 0)))
  (let ((clone (get-arg arguments "clone"))
	(username (get-arg arguments "username")))
    (unless clone
      (leave "The --clone argument is required."))
    (unless username
      (leave "The --username argument is required."))
    (let* ((clone-name (generate-clone-name clone))
	   (project (first (last (pathname-directory (cat (sb-unix:posix-getcwd) "/")))))
	   (project-remote-path (merge-pathnames project (cat "/home/" username "/" *prefix* "/"))))
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

(defun has-none-arg (args arg)
  "Returns T if the none arg was provided"
  (some #'(lambda (v) (string= (car v) arg)) args))

(defun say (message)
  "Outputs a message immediately on the screen"
  (format t message)
  (force-output))

(defun say-green (message)
  "Outputs a green message"
  (with-color (:green)
    (say message)))

(defun say-red (message)
  "Outputs a red message"
  (with-color (:red)
    (say message)))

(defun clean-stars (var)
  "Removes the stars from a string"
  (cl-ppcre:regex-replace-all "\\*" var ""))

(defun version ()
  "Returns the version"
  (cat "aergia " (load-time-value (sb-posix:getenv "VERSION"))))

(defun help ()
  "Returns the help"
  "Usage: aergia --clone BASE --username USERNAME [OPTIONS]

aergia uses short-lived containers to run tests on it.

Required arguments:

	--clone
		The base container to clone when creating testing containers.

	--username
		The username to use when connecting to the testing container.
		This username must have a password-less SSH authentication.

Options:

	--help
		Shows this help.

	--version
		Shows aergia's version.

	--default-shell
		Default value: /bin/bash
		Changes the default shell used to run commands.

	--prefix
		Default value: none
		Adds a prefix to the remote project path.

	--command
		Default value: make test
		Changes the commands to run the tests.

	--ssh-identity
		Default value: $HOME/.ssh/id_rsa
		Changes the identity used to connect to the test containers.

Online help: <https://github.com/Ralt/aergia>
For complete documentation, run: man aergia")
