# Toplevel makefile for etos v2.4

SUBDIRS = compiler bench bin

INSTALL_DIR = `pwd`/bin/

GSC = time gsc

.SUFFIXES:

all:

all-pre:

all-post:

install-pre:

install-post:

clean-pre:

clean-post:
	rm -f *~ hw.o* hw.scm

all-recursive install-recursive clean-recursive:
	for subdir in $(SUBDIRS); do \
	  target=`echo $@ | sed 's/-recursive//'`; \
	  install_dir="$(INSTALL_DIR)"; \
	  echo making $$target in $$subdir; \
	  (cd $$subdir && INSTALL_DIR="$$install_dir" GSC="$(GSC)" $(MAKE) $$target) || exit 1; \
	done

all: all-post

all-post: all-recursive

all-recursive: all-pre

install: install-post

install-post: install-recursive

install-recursive: install-pre

clean: clean-post

clean-post: clean-recursive

clean-recursive: clean-pre

bench: fake_target
	(cd bench && INSTALL_DIR="$$install_dir" GSC="$(GSC)" $(MAKE) bench) || exit 1

fake_target:
