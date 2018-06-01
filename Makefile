# Set personal or machine-local flags in a file named local.mk
ifneq ("$(wildcard local.mk)","")
include local.mk
endif

CCFLAGS = -O0 -Wall -g -I. -Ithird-party/ -fPIC
CFLAGS = $(CCFLAGS) -std=gnu11
CXXFLAGS = $(CCFLAGS) -std=c++11

SRCS = read-trace.c write-trace.c utils.c trace-db.cpp
OBJS := $(addsuffix .o,$(basename $(SRCS)))
DEPS := $(addsuffix .d,$(basename $(SRCS)))
TARGETS := libtrace-db.so sample

all: libtrace-db.so

libtrace-db.so: $(OBJS)
	$(CXX) $(CFLAGS) -o libtrace-db.so -fPIC -shared $(OBJS)  -Wl,-soname,libtrace-db.so

-include $(DEPS)

README.html: README.md
	pandoc $< -o $@

install: libtrace-db.so README.html
	install -Dm755 libtrace-db.so $(DESTDIR)lib/libtrace-db.so
	install -Dm644 README.html $(DESTDIR)share/doc/trace-db/README.html

# This target builds an Arch package from the current state of the
# repository by first rsync'ing it into the makepkg source directory
# then running makepkg to build a package.
local-makepkg:
	rm -rf /tmp/trace-db_pkg
	mkdir -p /tmp/trace-db_pkg
	rsync --exclude .git --exclude src -aruv ./ /tmp/trace-db_pkg
	rm -rf trace-db-git/src/
	mkdir -p trace-db-git/src/
	mv /tmp/trace-db_pkg trace-db-git/src/
	make -C trace-db-git/src/trace-db_pkg clean
	makepkg -ef

sample: libtrace-db.so sample.o
	$(CXX) $(CXXFLAGS) -o sample sample.o -ltrace-db -L.

unit-test: libtrace-db.so test/unit-test.o
	$(CXX) $(CXXFLAGS) -o unit-test test/unit-test.o -ltrace-db -L.

check: unit-test
	LD_LIBRARY_PATH=. ./unit-test

clean:
	rm -f *.o *.d libtrace-db.so README.html unit-test sample *.tar.xz
