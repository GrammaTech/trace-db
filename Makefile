CCFLAGS = -O0 -Wall -g -I. -fPIC
CFLAGS = $(CCFLAGS) -std=gnu11
CXXFLAGS = $(CCFLAGS) -std=c++11

SRCS = read-trace.c write-trace.c utils.c trace-db.cpp
OBJS := $(addsuffix .o,$(basename $(SRCS)))
DEPS := $(addsuffix .d,$(basename $(SRCS)))
TARGETS := libtrace-db.so sample

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
src/trace-db_pkg:
	mkdir -p $@

local-makepkg: src/trace-db_pkg
	rsync --exclude .git -aruv ./ src/trace-db_pkg
	make -C src/trace-db_pkg clean
	makepkg -ef

sample: libtrace-db.so sample.o
	$(CXX) $(CXXFLAGS) -o sample sample.o -ltrace-db -L.

unit-test: libtrace-db.so test/unit-test.o
	$(CXX) $(CXXFLAGS) -o unit-test test/unit-test.o -ltrace-db -L.

check: unit-test
	LD_LIBRARY_PATH=. ./unit-test

clean:
	rm -f *.o *.d libtrace-db.so README.html unit-test sample *.tar.xz
