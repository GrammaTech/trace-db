CFLAGS = -O0 -Wall -g

SRCS = read-trace.c write-trace.c

libtrace.so: $(SRCS) read-trace.h write-trace.h
	$(CC) $(CFLAGS) -o libtrace.so -fPIC -shared $(SRCS)  -Wl,-soname,libtrace.so

sample: libtrace.so sample.c
    $(CC) $(CFLAGS) -o sample sample.c -ltrace -L.


clean:
	rm libtrace.so trace-test
