CFLAGS = -O0 -Wall -g -std=gnu11 -I. -fPIC

SRCS = read-trace.c write-trace.c utils.c trace-db.c
OBJS := $(SRCS:%.c=%.o)
DEPS := $(SRCS:%.c=%.d)
TARGETS := libtrace-db.so sample

libtrace-db.so: $(OBJS)
    $(CC) $(CFLAGS) -o libtrace-db.so -fPIC -shared $(OBJS)  -Wl,-soname,libtrace-db.so

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
	$(CC) $(CFLAGS) -o sample sample.c -ltrace-db -L.

unit-test: libtrace-db.so test/unit-test.o
    $(CC) $(CFLAGS) -o unit-test test/unit-test.c -ltrace -L.

check: unit-test
	LD_LIBRARY_PATH=. ./unit-test

clean:
    rm -f *.o *.d libtrace-db.so README.html unit-test sample *.tar.xz
