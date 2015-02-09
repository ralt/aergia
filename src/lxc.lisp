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

(defun lxc-get-ip (name)
  "Gets the IP of a running LXC"
  (multiple-value-bind (_ groups)
      (cl-ppcre:scan-to-strings
       "IP:\\s*(\\d+\\.\\d+\\.\\d+\\.\\d+)"
       (clean-run
	   (cat "An error occured when getting information about " name ". Please review the error message above.")
	 "lxc-info" "--name" name))
    (declare (ignore _))
    (elt groups 0)))

(defun lxc-run-tests (username ip ignore-file prefix project command)
  "Runs tests in a container"
  ;; Just show that all the variables are available
  (format t "~A@~A ~A ~~/~A~A ~A~%" username ip ignore-file prefix project command))

(defun lxc-destroy (name)
  "Destroys an LXC"
  (clean-run
      (cat "An error occured when stopping " name ". Please review the error message above.")
    "lxc-stop" "--name" name)
  (clean-run
      (cat "An error occured when destroying " name ". Please review the error message above.")
    "lxc-destroy" "--name" name))
