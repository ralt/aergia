# aergia

Uses LXC to run short-lived containers and run tests on it.

## What

When you need to run tests in an isolated environment, aergia is for
you.

aergia will create a short-lived container based on another one, push
your code onto it, and run tests on it.

If the tests fail, the container is still there for you to debug. If
the tests pass, the containers used to run the tests will be deleted.

Ever wanted to run travis locally? Just setup the correct base
container for your project, and you'll be set.

## How

You have to define a base container, usually with the necessary
packages for the tests to be run. For example, most tests require
`make`.

You also need a password-less SSH authentication to this container.

As an example, here is how the base container for aergia is created:

```
$ lxc-create --name aergia \
	--template ubuntu -- \
	-S ~/.ssh/id_rsa.pub \
	--packages make,sbcl
```

aergia will then use this base and create an overlayfs-based container
to run tests on. Being overlayfs-based means that only delta changes
to the filesystem are written.

## Usage

```
$ aergia --help
Usage: aergia --clone BASE --username USERNAME [OPTIONS]

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
For complete documentation, run: man aergia
```

## Example session

Here is how I usually run my tests after writing some code:

```
florian@florian:~/common-lisp/aergia$ SSHKEY=~/.ssh/id_rsa sudo aergia --clone aergia --username ubuntu --prefix common-lisp
Cloning aergia... done.
Starting the short-lived container... done.
Trying to get the short-lived container's IP.... done.
Creating remote project folder... done.
Synchronizing sources... done.
Running tests...

This is SBCL 1.2.3.debian, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
To load "fiveam":
  Install 2 Quicklisp releases:
    alexandria fiveam
; Fetching #<URL "http://beta.quicklisp.org/archive/alexandria/2014-08-26/alexandria-20140826-git.tgz">
; 48.70KB
==================================================
49,872 bytes in 0.06 seconds (798.41KB/sec)
; Fetching #<URL "http://beta.quicklisp.org/archive/fiveam/2013-11-11/fiveam-1.2.tgz">
; 21.60KB
==================================================
22,116 bytes in 0.05 seconds (415.34KB/sec)
; Loading "fiveam"
[package alexandria.0.dev]........................
[package it.bese.fiveam].............
To load "aergia":
  Load 1 ASDF system:
    aergia
; Loading "aergia"
To load "external-program":
  Install 1 Quicklisp release:
    external-program
; Fetching #<URL "http://beta.quicklisp.org/archive/external-program/2014-12-17/external-program-20141217-git.tgz">
; 9.85KB
==================================================
10,082 bytes in 0.00 seconds (9845.70KB/sec)
; Loading "external-program"
[package external-program]..
; Loading "aergia"
To load "cl-ppcre":
  Install 1 Quicklisp release:
    cl-ppcre
; Fetching #<URL "http://beta.quicklisp.org/archive/cl-ppcre/2014-12-17/cl-ppcre-2.0.9.tgz">
; 155.87KB
==================================================
159,611 bytes in 0.47 seconds (328.15KB/sec)
; Loading "cl-ppcre"
[package cl-ppcre]................................
............................
; Loading "aergia"
[package getopt]..................................
[package anaphora]................................
[package anaphora-basic]..........................
[package anaphora-symbol].........................
[package let-plus]................................
[package cl-colors]...............................
[package cl-ansi-text]............................
[package aergia].
; compiling file "/home/ubuntu/common-lisp/aergia/test/test-suites.lisp" (written 10 FEB 2015 07:48:11 PM):
; compiling (DEFPACKAGE #:AERGIA-TEST ...)
; compiling (IN-PACKAGE #:AERGIA-TEST)
; compiling (IT.BESE.FIVEAM:DEF-SUITE AERGIA)
; compiling (DEFMETHOD ASDF/ACTION:PERFORM ...)

; /home/ubuntu/.cache/common-lisp/sbcl-1.2.3.debian-linux-x64/home/ubuntu/common-lisp/aergia/test/test-suites-TMP.fasl written
; compilation finished in 0:00:00.005
; compiling file "/home/ubuntu/common-lisp/aergia/test/aergia.lisp" (written 10 FEB 2015 07:48:11 PM):
; compiling (IN-PACKAGE #:AERGIA-TEST)
; compiling (IT.BESE.FIVEAM:IN-SUITE AERGIA)
; compiling (IT.BESE.FIVEAM:TEST CAT ...)
; compiling (IT.BESE.FIVEAM:TEST CAT-WITH-SPACES ...)
; compiling (IT.BESE.FIVEAM:TEST GET-ARG ...)
; compiling (IT.BESE.FIVEAM:TEST HAS-NONE-ARG ...)
; compiling (IT.BESE.FIVEAM:TEST CLEAN-STARS ...)
; compiling (IT.BESE.FIVEAM:TEST HELP ...)

; /home/ubuntu/.cache/common-lisp/sbcl-1.2.3.debian-linux-x64/home/ubuntu/common-lisp/aergia/test/aergia-TMP.fasl written
; compilation finished in 0:00:00.003

Running test suite AERGIA
 Running test CAT .
 Running test CAT-WITH-SPACES .
 Running test GET-ARG ..
 Running test HAS-NONE-ARG .
 Running test CLEAN-STARS .
 Running test HELP .
 Did 7 checks.
    Pass: 7 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)


Tests pass!

Cleaning up the test containers... done.
florian@florian:~/common-lisp/aergia$
```

## Installation

- Download the sources and run `make && make install`. Only `sbcl` is
  needed.
- Download and install the [rpm
  file](https://github.com/Ralt/aergia/releases/download/1.0.0/aergia-1.0.0-1.x86_64.rpm)
  ([pgp signature](https://github.com/Ralt/aergia/releases/download/1.0.0/aergia_1.0.0-rpm.sig))
  if you're on Fedora/CentOS/Red Hat (x86~64~ only)
- Download and install the [deb
  file](https://github.com/Ralt/aergia/releases/download/1.0.0/aergia_1.0.0_amd64.deb)
  ([pgp signature](https://github.com/Ralt/aergia/releases/download/1.0.0/aergia_1.0.0-deb.sig))
  if you're on Ubuntu/Debian (amd64 only)

## More

Either read the [online manual](manpage.md), or run `man aergia` for a
more complete documentation.

## License

The code is licensed under the MIT license. See the [LICENSE](LICENSE) file.
