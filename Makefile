BASEDIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

# Set personal or machine-local flags in a file named local.mk
ifneq ("$(wildcard local.mk)","")
include local.mk
endif

HEADERS = MemoryMap.hpp \
          QueryObjects.hpp \
          TraceBufferSize.hpp \
          TraceDB.hpp \
          TraceError.hpp \
          Trace.hpp \
          TracePoint.hpp \
          TraceVarInfo.hpp \
          TypeDescription.hpp \
          Utils.hpp \
          lisp/trace.h \
          lisp/trace-db.h
SRCS = lisp/trace.cpp lisp/trace-db.cpp
OBJS := $(addsuffix .o,$(basename $(SRCS)))
LIBTRACEDB_SO := lisp/libtrace-db.so

BOOST_BASIC_ARCHIVE_HEADER ?= boost/archive/basic_archive.hpp
ifneq ($(wildcard /usr/local/include/$(BOOST_BASIC_ARCHIVE_HEADER)),)
BOOST_INCLUDE_DIR ?= /usr/local/include/
BOOST_LIB_DIR ?= /usr/local/lib/
else ifneq ($(wildcard /usr/include/$(BOOST_BASIC_ARCHIVE_HEADER)),)
BOOST_INCLUDE_DIR ?= /usr/include/
BOOST_LIB_DIR ?= /usr/lib/
else
BOOST_INCLUDE_DIR ?= third-party/boost_1_67_0/include/
BOOST_LIB_DIR ?= third-party/boost_1_67_0/lib/
endif
BOOST_SERIALIZATION := $(BOOST_INCLUDE_DIR)/$(BOOST_BASIC_ARCHIVE_HEADER)

CXXFLAGS = -O3 -Wall -I. -I$(BOOST_INCLUDE_DIR) -fPIC -std=c++11

all: $(LIBTRACEDB_SO)

$(BOOST_SERIALIZATION):
	mkdir -p $(BASEDIR)/third-party && \
	cd $(BASEDIR)/third-party && \
	wget https://dl.bintray.com/boostorg/release/1.67.0/source/boost_1_67_0.tar.bz2 && \
	tar --bzip2 -xf boost_1_67_0.tar.bz2 && \
	cd boost_1_67_0/ && \
	./bootstrap.sh --prefix=$(BASEDIR)/third-party/boost_1_67_0/ && \
	./bjam --with-iostreams --with-system --with-serialization cxxflags=-fPIC link=static variant=release install

$(OBJS): $(HEADERS) $(BOOST_SERIALIZATION)

$(LIBTRACEDB_SO): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $(LIBTRACEDB_SO) -fPIC -shared $(OBJS) -L $(BOOST_LIB_DIR) -lboost_iostreams -lboost_system -lboost_serialization -Wl,-soname,$(LIBTRACEDB_SO)

README.html: README.md
	pandoc $< -o $@

install: $(LIBTRACEDB_SO) README.html
	install -Dm755 $(LIBTRACEDB_SO) $(DESTDIR)lib/libtrace-db.so
	install -Dm644 README.html $(DESTDIR)share/doc/trace-db/README.html

src/trace-db_pkg:
	mkdir -p src/
	ln -s ../ $@

# This target builds an Arch package from the current state of the repo.
local-makepkg: PKGBUILD $(wildcard *.hpp) $(wildcard lisp/*.lisp) $(wildcard lisp/*.h) $(wildcard lisp/*.cpp) src/trace-db_pkg
	makepkg -ef
	rm -f src/trace-db_pkg; rmdir src/

sample.o: $(HEADERS)

sample: sample.o
	$(CXX) $(CXXFLAGS) -o sample sample.o -pthread -L $(BOOST_LIB_DIR) -lboost_iostreams -lboost_system -lboost_serialization

test/unit-test.o: $(HEADERS)

unit-test: test/unit-test.o
	$(CXX) $(CXXFLAGS) -o unit-test test/unit-test.o -pthread -L $(BOOST_LIB_DIR) -lboost_iostreams -lboost_system -lboost_serialization

check: unit-test
	./unit-test

clean:
	rm -f lisp/*.o test/*.o *.o $(LIBTRACEDB_SO) README.html unit-test sample *.tar.xz
