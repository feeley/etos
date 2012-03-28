# Makefile for etos compiler

.SUFFIXES:
.SUFFIXES: .scm

CC_OPTIONS = -cc-options "-U___SINGLE_HOST"
#CC_OPTIONS = 

all: etos estart

install: all
	cp etos "$(INSTALL_DIR)"
	cp estart "$(INSTALL_DIR)"
	cp rt-gambit.scm "$(INSTALL_DIR)"

clean:
	rm -rf etos etos_.c etos.c estart estart_.c estart.c build-parser erlang.scm build-parser_.c build-parser.c *~ *.o*

etos: etos.scm rt-gambit.scm rt-lib.scm port-generic.scm ks.scm utils.scm error.scm bifs.scm parser.scm erlang.scm scanner.scm ast-pp.scm ast-comp.scm ast-build.scm ast-analyze.scm scm-build.scm pm-comp.scm pm-build.scm pm-predict.scm
	$(GSC) -exe $(CC_OPTIONS) -o $@ -e "(define install-dir \"$(INSTALL_DIR)\")" $@.scm

estart: estart.scm rt-gambit.scm rt-lib.scm port-generic.scm ks.scm
	$(GSC) -exe $(CC_OPTIONS) -o $@ $@.scm

build-parser: build-parser.scm
	$(GSC) -exe $(CC_OPTIONS) -o $@ $@.scm

erlang.scm: build-parser grammar.scm
	./build-parser
