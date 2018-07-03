BASEDIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

# Set personal or machine-local flags in a file named local.mk
ifneq ("$(wildcard local.mk)","")
include local.mk
endif

SRCS = read-trace.cpp write-trace.cpp utils.cpp trace-db.cpp
OBJS := $(addsuffix .o,$(basename $(SRCS)))
DEPS := $(addsuffix .d,$(basename $(SRCS)))

BOOST_BASIC_ARCHIVE_HEADER ?= boost/archive/basic_archive.hpp
ifneq ($(wildcard /usr/local/include/$(BOOST_BASIC_ARCHIVE_HEADER)),)
BOOST_INCLUDE_DIR ?= /usr/local/include/
BOOST_LIB_DIR ?= /usr/local/lib/
else
BOOST_INCLUDE_DIR ?= third-party/boost_1_67_0/include/
BOOST_LIB_DIR ?= third-party/boost_1_67_0/lib/
endif
BOOST_SERIALIZATION := $(BOOST_INCLUDE_DIR)/$(BOOST_BASIC_ARCHIVE_HEADER)

CXXFLAGS = -O0 -Wall -g -I. -I$(BOOST_INCLUDE_DIR) -fPIC -std=c++11

all: libtrace-db.so

$(BOOST_SERIALIZATION):
	mkdir -p $(BASEDIR)/third-party && \
	cd $(BASEDIR)/third-party && \
	wget https://dl.bintray.com/boostorg/release/1.67.0/source/boost_1_67_0.tar.bz2 && \
	tar --bzip2 -xf boost_1_67_0.tar.bz2 && \
	cd boost_1_67_0/ && \
	./bootstrap.sh --prefix=$(BASEDIR)/third-party/boost_1_67_0/ && \
	./bjam --with-serialization cxxflags=-fPIC link=static install

$(OBJS): $(BOOST_SERIALIZATION)

libtrace-db.so: $(OBJS)
	$(CXX) $(CXXFLAGS) -o libtrace-db.so -fPIC -shared $(OBJS) -L $(BOOST_LIB_DIR) -lboost_serialization -Wl,-soname,libtrace-db.so

-include $(DEPS)

README.html: README.md
	pandoc $< -o $@

install: libtrace-db.so README.html
	install -Dm755 libtrace-db.so $(DESTDIR)lib/libtrace-db.so
	install -Dm644 README.html $(DESTDIR)share/doc/trace-db/README.html

# This target builds an Arch package from the current state of the
# repository by first rsync'ing it into the makepkg source directory
# then running makepkg to build a package.
local-makepkg:
	rm -rf /tmp/trace-db_pkg
	mkdir -p /tmp/trace-db_pkg
	rsync --exclude .git --exclude src -aruv ./ /tmp/trace-db_pkg
	rm -rf trace-db-git/src/
	mkdir -p trace-db-git/src/
	mv /tmp/trace-db_pkg trace-db-git/src/
	make -C trace-db-git/src/trace-db_pkg clean
	makepkg -ef

sample: libtrace-db.so sample.o
	$(CXX) $(CXXFLAGS) -o sample sample.o -L $(BOOST_LIB_DIR) -lboost_serialization -L. -ltrace-db

unit-test: libtrace-db.so test/unit-test.o
	$(CXX) $(CXXFLAGS) -o unit-test test/unit-test.o -L $(BOOST_LIB_DIR) -lboost_serialization -L. -ltrace-db

check: unit-test
	LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:. ./unit-test

clean:
	rm -f *.o *.d libtrace-db.so README.html unit-test sample *.tar.xz
