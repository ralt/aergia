(in-package #:aergia)

(defun lxc-clone (base clone-name)
  "Clones an LXC with an overlayfs backing store"
  (say (cat "Cloning " base "... "))
  (clean-run
      (cat "An error occured when cloning " base ".")
    "lxc-clone" "--snapshot"
    "--orig" base
    "--new" clone-name
    "--backingstore" "overlayfs")
  (say-green "done.~%"))

(defun lxc-start (name)
  "Starts an LXC"
  (say "Starting the short-lived container... ")
  (clean-run
      (cat "An error occured when starting " name ".")
    "lxc-start" "--name" name)
  (say-green "done.~%"))

(defun lxc-get-ip (name &optional (acc 0))
  "Gets the IP of a running LXC"
  (when (= acc 20)
    (leave "Could not retrieve the container's IP after 20 trials. Please check the configuration."))
  (if (= acc 0)
      (say "Trying to get the short-lived container's IP.")
      (say "."))
  (multiple-value-bind (_ groups)
      (cl-ppcre:scan-to-strings
       "IP:\\s*(\\d+\\.\\d+\\.\\d+\\.\\d+)"
       (clean-run
	   (cat "An error occured when getting information about " name ".")
	 "lxc-info" "--name" name))
    (declare (ignore _))
    ;; Leave enough time for:
    ;; - if the IP is found, for ssh to start
    ;; - if the IP is not found, for another trial
    (sleep 1)
    (if (> (length groups) 0)
	(progn
	  (say-green " done.~%")
	  (elt groups 0))
	(lxc-get-ip name (1+ acc)))))

(defun lxc-synchronize-project (username ip project-remote-path ssh-identity)
  "Synchronizes the project sources with a defined folder in the container"
  (say "Creating remote project folder... ")
  (clean-run
      (cat "An error occured when connecting to " username "@" ip ".")
    "ssh" "-o" "StrictHostKeyChecking=no" "-i" ssh-identity (cat username "@" ip) "mkdir" "-p" (namestring project-remote-path))
  (say-green "done.~%")
  (say "Synchronizing sources... ")
  (clean-run
      (cat "There was an error synchronizing with " username "@" ip ".")
    "scp" "-o" "StrictHostKeyChecking=no" "-i" "/home/florian/.ssh/id_rsa" "-r" "." (cat username "@" ip ":" (namestring project-remote-path)))
  (say-green "done.~%"))

(defun lxc-run-tests (username ip project-remote-path ssh-identity command)
  "Runs tests in a container"
  (say "Running tests... ")
  (clean-run
      (cat "An error occured when running tests on " username "@" ip ":" (namestring project-remote-path) ".")
    "ssh" "-o" "StrictHostKeyChecking=no" "-i" ssh-identity (cat username "@" ip) "cd" project-remote-path ";" command)
  (say-green "done.~%"))

(defun lxc-cleanup (name)
  "Finds all the test-NAME-* containers and destroy them"
  (say "Cleaning up... ")
  (mapcar #'lxc-destroy
	  (cl-ppcre:all-matches-as-strings
	   (cat "test-" name "-\\d+")
	   (clean-run
	       "An error occured when trying to list containers."
	     "lxc-ls")))
  (say-green "done.~%"))

(defun lxc-destroy (name)
  "Destroys an LXC"
  (clean-run
      (cat "An error occured when stopping " name ".")
    "lxc-stop" "--name" name)
  (clean-run
      (cat "An error occured when destroying " name ".")
    "lxc-destroy" "--name" name))
