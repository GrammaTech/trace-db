CFLAGS = -O0 -Wall -g

SRCS = read-trace.c write-trace.c

libtrace.so: $(SRCS) read-trace.h write-trace.h
	$(CC) $(CFLAGS) -o libtrace.so -fPIC -shared $(SRCS)  -Wl,-soname,libtrace.so

trace-test: libtrace.so test.c
	$(CC) $(CFLAGS) -o trace-test test.c -ltrace -L.

clean:
	rm libtrace.so trace-test
