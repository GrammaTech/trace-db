%.o: %.cpp
	clang++ -std=c++11 -c -o $@ $<

foo: foo.o bar.o
	clang++ $^ -o $@

clean:
	rm -f *.o foo
