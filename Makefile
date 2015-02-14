APP_NAME=aergia
LISP_FILES=$(shell find . -name '*.lisp')
ASDF_TREE ?= ~/quicklisp/
DIST_FOLDER ?= dist/root/usr/bin
APP_OUT=$(DIST_FOLDER)/aergia
QL_LOCAL=$(PWD)/.quicklocal/quicklisp
QUICKLISP_SCRIPT=http://beta.quicklisp.org/quicklisp.lisp
LOCAL_OPTS=--noinform --noprint --disable-debugger --no-sysinit --no-userinit
QL_OPTS=--load $(QL_LOCAL)/setup.lisp
LISP ?= sbcl
SOURCES := $(wildcard src/*.lisp) $(wildcard *.asd)
BUILDAPP = ./bin/buildapp
TEST_SOURCES=$(shell find test/ -name '*.lisp')
WITH_DOCS ?= 1

.PHONY: clean install release deb rpm test man create-base-container aergia-test

all: $(APP_OUT)

create-base-container:
ifndef SSHKEY
	$(error SSHKEY needs to be provided. It must be the path to the public SSH key.)
endif
	@lxc-create --name aergia \
		--template ubuntu -- \
		-S $(SSHKEY) \
		--packages make,sbcl

aergia-test:
ifndef SSHKEY
	$(error SSHKEY needs to be provided. It must be the path of the private SSH key.)
endif
	@aergia --clone aergia --username ubuntu --prefix common-lisp --ssh-identity $(SSHKEY)

test: $(TEST_SOURCES) $(QL_LOCAL)/setup.lisp install-deps
	@sbcl $(QL_OPTS) \
		--eval '(ql:quickload :fiveam)' \
		--eval '(setf fiveam:*debug-on-error* t)' \
		--eval '(setf fiveam:*debug-on-failure* t)' \
		--eval '(setf *debugger-hook* (lambda (c h) (declare (ignore c h)) (uiop:quit -1)))' \
		--eval '(ql:quickload :aergia)' \
		--eval '(asdf:test-system :aergia)' \
		--quit

release:
ifndef VERSION
	$(error VERSION needs to be provided.)
endif
	make clean
	make
	make man
	make deb
	make rpm

man:
ifeq ($(WITH_DOCS),1)
	mkdir -p dist/root/usr/share/man/man1/
	pandoc -s -t man manpage.md > dist/root/usr/share/man/man1/$(APP_NAME).1
	gzip dist/root/usr/share/man/man1/$(APP_NAME).1
endif

deb: $(APP_OUT)
	@fpm -p dist/ \
		-d "lxc (>= 1.0)" \
		-s dir -t deb -n $(APP_NAME) -v $(VERSION) -C dist/root usr
	@gpg --output dist/$(APP_NAME)_$(VERSION)-deb.sig \
		--detach-sig dist/$(APP_NAME)_$(VERSION)_amd64.deb

rpm: $(APP_OUT)
	@fpm -p dist/ \
		-d "lxc" \
		-s dir -t rpm -n $(APP_NAME) -v $(VERSION) -C dist/root usr
	@gpg --output dist/$(APP_NAME)_$(VERSION)-rpm.sig \
		--detach-sig dist/$(APP_NAME)-$(VERSION)-1.x86_64.rpm

install: man $(APP_OUT)
	install $(APP_OUT) $(DESTDIR)/usr/bin
	install -g 0 -o 0 -m 0644 dist/root/usr/share/man/man1/$(APP_NAME).1.gz /usr/share/man/man1/

bin:
	@mkdir bin

clean:
	@-yes | rm -rf $(QL_LOCAL)
	@-rm -f $(APP_OUT) deps install-deps
	@-rm -f dist/aergia*

$(QL_LOCAL)/setup.lisp:
	@curl -O $(QUICKLISP_SCRIPT)
	@sbcl $(LOCAL_OPTS) \
		--load quicklisp.lisp \
		--eval '(quicklisp-quickstart:install :path "$(QL_LOCAL)")' \
		--eval '(quit)'

deps:
	@sbcl $(LOCAL_OPTS) $(QL_OPTS) \
             --eval '(push "$(PWD)/" asdf:*central-registry*)' \
             --eval '(ql:quickload :aergia)' \
             --eval '(quit)'
	@touch $@

install-deps: $(QL_LOCAL)/setup.lisp deps
	@touch $@

bin/buildapp: bin $(QL_LOCAL)/setup.lisp
	@cd $(shell sbcl $(LOCAL_OPTS) $(QL_OPTS) \
				--eval '(ql:quickload :buildapp :silent t)' \
				--eval '(format t "~A~%" (asdf:system-source-directory :buildapp))' \
				--eval '(quit)') && \
	$(MAKE) DESTDIR=$(PWD) install

$(APP_OUT): $(SOURCES) bin/buildapp $(QL_LOCAL)/setup.lisp install-deps
	@mkdir -p $(DIST_FOLDER)
	@$(BUILDAPP) --logfile /tmp/build.log \
			--sbcl sbcl \
			--asdf-path . \
			--asdf-tree $(QL_LOCAL)/local-projects \
			--asdf-tree $(QL_LOCAL)/dists \
			--asdf-path . \
			--load-system $(APP_NAME) \
			--entry $(APP_NAME):main \
			--compress-core \
			--output $(APP_OUT)
