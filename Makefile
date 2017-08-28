CFLAGS = -O2 -Wall

libtrace.so: trace.c trace.h
	$(CC) $(CFLAGS) -o libtrace.so -fPIC -shared trace.c -Wl,-soname,libtrace.so

trace-test: libtrace.so test.c
	$(CC) $(CFLAGS) -o trace-test test.c -ltrace -L.
