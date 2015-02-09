(in-package #:aergia)

(defun lxc-clone (base clone-name)
  "Clones an LXC with an overlayfs backing store"
  (clean-run
      (cat "An error occured when cloning " base ". Please review the error message above.")
    "lxc-clone" "--snapshot"
    "--orig" base
    "--new" clone-name
    "--backingstore" "overlayfs"))

(defun lxc-start (name)
  "Starts an LXC"
  (clean-run
      (cat "An error occured when starting " name ". Please review the error message above.")
    "lxc-start" "--name" name))

(defun lxc-get-ip (name &optional (acc 0))
  "Gets the IP of a running LXC"
  (when (= acc 20)
    (leave "Could not retrieve the container's IP after 20 trials. Please check the configuration."))
  (multiple-value-bind (_ groups)
      (cl-ppcre:scan-to-strings
       "IP:\\s*(\\d+\\.\\d+\\.\\d+\\.\\d+)"
       (clean-run
	   (cat "An error occured when getting information about " name ". Please review the error message above.")
	 "lxc-info" "--name" name))
    (declare (ignore _))
    ;; Leave enough time for:
    ;; - if the IP is found, for ssh to start
    ;; - if the IP is not found, for another trial
    (sleep 1)
    (if (> (length groups) 0)
	(elt groups 0)
	(lxc-get-ip name (1+ acc)))))

(defun lxc-synchronize-project (username ip project-remote-path ssh-identity)
  "Synchronizes the project sources with a defined folder in the container"
  (clean-run
      (cat "An error occured when connecting to " username "@" ip ". Please review the error message above.")
    "ssh" "-o" "StrictHostKeyChecking=no" "-i" ssh-identity (cat username "@" ip) "mkdir" "-p" (namestring project-remote-path))
  (clean-run
      (cat "There was an error synchronizing with " username "@" ip ". Please review the error message above.")
    "scp" "-o" "StrictHostKeyChecking=no" "-i" "/home/florian/.ssh/id_rsa" "-r" "." (cat username "@" ip ":" (namestring project-remote-path))))

(defun lxc-run-tests (username ip project-remote-path ssh-identity command)
  "Runs tests in a container"
  (clean-run
      (cat "An error occured when running tests on " username "@" ip ". Please review the error message above.")
    "ssh" "-o" "StrictHostKeyChecking=no" "-i" ssh-identity (cat username "@" ip) "cd" project-remote-path ";" command))

(defun lxc-cleanup (name)
  "Finds all the test-NAME-* containers and destroy them"
  (mapcar #'lxc-destroy
	  (cl-ppcre:all-matches-as-strings
	   (cat "test-" name "-\\d+")
	   (clean-run
	       "An error occured when trying to list containers. Please review the error message above."
	     "lxc-ls"))))

(defun lxc-destroy (name)
  "Destroys an LXC"
  (clean-run
      (cat "An error occured when stopping " name ". Please review the error message above.")
    "lxc-stop" "--name" name)
  (clean-run
      (cat "An error occured when destroying " name ". Please review the error message above.")
    "lxc-destroy" "--name" name))
