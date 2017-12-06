CFLAGS = -O0 -Wall -g -std=gnu11

SRCS = read-trace.c write-trace.c

libtrace-db.so: $(SRCS) read-trace.h write-trace.h
	$(CC) $(CFLAGS) -o libtrace-db.so -fPIC -shared $(SRCS)  -Wl,-soname,libtrace-db.so

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

sample: libtrace-db.so sample.c
	$(CC) $(CFLAGS) -o sample sample.c -ltrace-db -L.

unit-test: libtrace-db.so test/unit-test.c
	$(CC) $(CFLAGS) -o unit-test test/unit-test.c -ltrace-db -L. -I.

check: unit-test
	LD_LIBRARY_PATH=. ./unit-test

clean:
	rm -f libtrace-db.so README.html unit-test sample *.tar.xz
