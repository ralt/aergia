% AERGIA(1) aergia man page
% Florian Margaine <florian@margaine.com>
% February 10, 2015

# NAME

aergia - Uses LXC to run short-lived containers and run tests on it.

# SYNOPSIS

aergia --clone BASE --username USERNAME [OPTIONS]

# DESCRIPTION

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

# HELP

Get help and report bugs at <https://github.com/Ralt/aergia>

# DETAILS

This section explains the details of how aergia works.

Here is each step run by aergia, not taking arguments' parsing in
account:

- generates a name for the clone based on this template: `test-NAME-TIMESTAMP`
- runs `lxc-clone` using the overlayfs backing store.
- starts the newly-created clone
- gets the IP of the clone
- sends the file of the project to the clone
- runs the command (by default, `make test`) in the remote project

Now, 2 possible choices:

- if the tests fail, stop immediately, which lets the user connect to
  the container and debug
- if the tests pass, delete all the `test-NAME-*` containers, which
  means that previous failures' containers will be deleted too
