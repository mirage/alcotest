VERSION = $(shell git describe --match 'v[0-9]*' --dirty='.m' --always)
VFILE   = lib/alcotest_version.ml
PREFIX ?= $(shell opam config var prefix)

SETUP = ocaml setup.ml

build: setup.data $(VFILE)
	$(SETUP) -build $(BUILDFLAGS)

all: setup.data
	$(SETUP) -all $(ALLFLAGS)

setup.ml: _oasis
	rm -f _tags myocamlbuild.ml
	oasis setup
	echo 'true: debug, bin_annot' >> _tags
	echo 'true: warn_error(+1..49-3), warn(A-4-41-44)' >> _tags
#	echo 'Ocamlbuild_plugin.mark_tag_used "tests"' >> myocamlbuild.ml

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test:
	$(SETUP) -configure --enable-tests --prefix $(PREFIX)
	$(MAKE) build
	$(SETUP) -test $(TESTFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	if [ -f setup.ml ]; then $(SETUP) -clean $(CLEANFLAGS); fi
	rm -f setup.data setup.ml myocamlbuild.ml _tags configure
	rm -f lib/*.odocl lib/META setup.log lib/*.mldylib lib/*.mllib
	rm -rf examples/*.byte examples/_tests
	rm -f $(VFILE)
	rm -rf $(APP) _tests

setup.data: setup.ml
	$(SETUP) -configure --prefix $(PREFIX)

$(VFILE):
	echo "let v = \"$(VERSION)\"" > $(VFILE)

doc/html/.git:
	cd doc/html && (\
		git init && \
		git remote add origin git@github.com:mirage/alcotest.git && \
		git checkout -b gh-pages \
	)

gh-pages: doc/html/.git doc
	cd doc/html && git checkout gh-pages
	rm -f doc/html/*.html
	cp alcotest.docdir/*.html doc/html/
	cd doc/html && git add *.html
	cd doc/html && git commit -a -m "Doc updates"
	cd doc/html && git push origin gh-pages

.PHONY: build doc test all install uninstall reinstall clean distclean


RVERSION = $(shell grep 'Version:' _oasis | sed 's/Version: *//')
RNAME    = $(shell grep 'Name:' _oasis    | sed 's/Name: *//')
RARCHIVE = https://github.com/mirage/$(RNAME)/archive/$(RVERSION).tar.gz

release:
	git tag -a $(RVERSION) -m "Version $(RVERSION)."
	git push upstream $(RVERSION)
	$(MAKE) pr

pr:
	opam publish prepare $(RNAME).$(RVERSION) $(RARCHIVE)
	OPAMYES=1 opam publish submit $(RNAME).$(RVERSION) && \
	  rm -rf $(RNAME).$(RVERSION)
