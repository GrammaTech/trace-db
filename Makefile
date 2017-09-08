CFLAGS = -O0 -Wall -g

SRCS = read-trace.c write-trace.c

libtrace.so: $(SRCS) read-trace.h write-trace.h
	$(CC) $(CFLAGS) -o libtrace.so -fPIC -shared $(SRCS)  -Wl,-soname,libtrace.so

sample: libtrace.so sample.c
	$(CC) $(CFLAGS) -o sample sample.c -ltrace -L.

unit-test: libtrace.so test/unit-test.c
	$(CC) $(CFLAGS) -o unit-test test/unit-test.c -ltrace -L. -I.

check: unit-test
	LD_LIBRARY_PATH=. ./unit-test

clean:
	rm libtrace.so unit-test sample
