# aergia

Uses LXC to run short-lived containers and run tests on it.

No code yet.

## Roadmap

Will support:

- Based on templates
- Based on clones

The container needs to have at least: `rsync`, `ssh`

Your container (whether it's based on a template or is a clone) will
need to have the required configuration installed. For example, it's
often a good idea to have sudo NOPASSWD for the test runner
user. Another example is installing the necessary packages once,
instead of every time you run the tests.

If you want to mimic travis CI, it's possible to mimic a default
container based on the list of packages installed by default provided
[here](http://docs.travis-ci.com/user/ci-environment/). However, be
aware that `aergia` doesn't handle the `.travis.yml` file. (Which
means that if your testing depends on settings defined in it, you have
to set them manually in the `aergia` process.)

```
lxc-create -n test-PROJECT-TIMESTAMP -B overlayfs -t TEMPLATE -- -S SSH-KEY
lxc-start -n test-PROJECT-TIMESTAMP
lxc-info -n test-PROJECT-TIMESTAMP | grep 'IP:' | awk '{print $2;}' # get $IP
ssh TEMPLATE-USERNAME@$IP "mkdir -p ~/[PREFIX]PROJECT" && rsync -avz --filter=':- IGNORE_FILE' PROJECT-PATH TEMPLATE-USERNAME@$IP:~/[PREFIX]PROJECT && ssh TEMPLATE-USERNAME@$IP "cd ~/[PREFIX]PROJECT; TEST-COMMAND"
if [ $? == 0 ]; then
	lxc-stop test-PROJECT-TIMESTAMP
	lxc-destroy test-PROJECT-TIMESTAMP
	aergia cleanup PROJECT
fi
```

For example, with the following variables:

- TEMPLATE: `ubuntu`
- PROJECT-TIMESTAMP: `lxc-wrapper-20150208120112`
- SSH-KEY: `~/.ssh/id_rsa.pub`
- TEMPLATE-USERNAME: `ubuntu`
- PREFIX: `common-lisp/`
- PROJECT: `lxc-wrapper`
- TEST-COMMAND: `make test`
- IGNORE-FILE: `.gitignore`
- PROJECT-PATH: `.`

```
lxc-create -n test-lxc-wrapper-20150208120112 -B overlayfs -t ubuntu -- -S ~/.ssh/id_rsa.pub
lxc-start -n test-lxc-wrapper-20150208120112
lxc-info -n test-lxc-wrapper-20150208120112 | grep 'IP:' | awk '{print $2;}' # get $IP
ssh ubuntu@$IP "mkdir -p ~/common-lisp/lxc-wrapper" \
	&& rsync -avz --filter=':- .gitignore' . ubuntu@$IP:~/common-lisp/lxc-wrapper \
	&& ssh ubuntu@$IP "cd ~/common-lisp/lxc-wrapper; make test"
if [ $? == 0 ]; then
	lxc-stop test-lxc-wrapper-20150208120112
	lxc-destroy test-lxc-wrapper-20150208120112
	aergia cleanup PROJECT
fi
```

`aergia cleanup PROJECT` looks for all the remnants containers of
`PROJECT` and deletes them.

This means that as long as the tests fail, you can connect to the
container and debug the failure.

An option shall exist to *not* delete the container. You have to think
about executing `aergia cleanup PROJECT` manually though, or run
passing tests without the option.

Required:

- (or (and "template" "template arguments (i.e. (or `-S ~/.ssh/id_rsa.pub`
                                                    `--username=foo --password=bar`)"))
      "clone")
- TEMPLATE-USERNAME (except if specified in template arguments)

Default values:

- PROJECT: `basename $PWD`
- TEST-COMMAND: `make test`

Overridable values: PROJECT, TEST-COMMAND, PREFIX, IGNORE-FILE, PROJECT-PATH

## To be done

Personal todo on the project.

- Better output for tests
- Write tests
- Rewrite the README.
