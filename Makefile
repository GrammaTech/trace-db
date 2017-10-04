CFLAGS = -O0 -Wall -g

SRCS = read-trace.c write-trace.c

libtrace.so: $(SRCS) read-trace.h write-trace.h
	$(CC) $(CFLAGS) -o libtrace.so -fPIC -shared $(SRCS)  -Wl,-soname,libtrace.so

README.html: README.md
	pandoc $< -o $@

install: libtrace.so README.html
	install -Dm755 libtrace.so $(DESTDIR)lib/libtrace.so
	install -Dm644 README.html $(DESTDIR)share/doc/libtrace/README.html

# This target builds an Arch package from the current state of the
# repository by first rsync'ing it into the makepkg source directory
# then running makepkg to build a package.
src/libtrace_pkg:
	mkdir -p $@

local-makepkg: src/libtrace_pkg
	rsync --exclude .git -aruv ./ src/libtrace_pkg
	make -C src/libtrace_pkg clean
	makepkg -ef

sample: libtrace.so sample.c
	$(CC) $(CFLAGS) -o sample sample.c -ltrace -L.

unit-test: libtrace.so test/unit-test.c
	$(CC) $(CFLAGS) -o unit-test test/unit-test.c -ltrace -L. -I.

check: unit-test
	LD_LIBRARY_PATH=. ./unit-test

clean:
	rm -f libtrace.so README.html unit-test sample *.tar.xz
