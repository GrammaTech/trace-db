BASEDIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
PACKAGE_NAME := trace-db

# Set personal or machine-local flags in a file named local.mk
ifneq ("$(wildcard local.mk)","")
include local.mk
endif


## Build the trace database
TRACE_DB_CFFI_INTERFACE_DIR=cffi-interface
TRACE_DB_HEADERS_DIR=$(TRACE_DB_CFFI_INTERFACE_DIR)/trace-db
HEADERS = $(TRACE_DB_HEADERS_DIR)/MemoryMap.hpp \
          $(TRACE_DB_HEADERS_DIR)/QueryObjects.hpp \
          $(TRACE_DB_HEADERS_DIR)/TraceBufferSize.hpp \
          $(TRACE_DB_HEADERS_DIR)/TraceDB.hpp \
          $(TRACE_DB_HEADERS_DIR)/TraceError.hpp \
          $(TRACE_DB_HEADERS_DIR)/Trace.hpp \
          $(TRACE_DB_HEADERS_DIR)/TracePoint.hpp \
          $(TRACE_DB_HEADERS_DIR)/TraceVarInfo.hpp \
          $(TRACE_DB_HEADERS_DIR)/TypeDescription.hpp \
          $(TRACE_DB_HEADERS_DIR)/Utils.hpp \
          $(TRACE_DB_CFFI_INTERFACE_DIR)/trace.h \
          $(TRACE_DB_CFFI_INTERFACE_DIR)/trace-db.h
SRCS = $(TRACE_DB_CFFI_INTERFACE_DIR)/trace.cpp \
       $(TRACE_DB_CFFI_INTERFACE_DIR)/trace-db.cpp
OBJS := $(addsuffix .o,$(basename $(SRCS)))
LIBTRACEDB_SO := $(TRACE_DB_CFFI_INTERFACE_DIR)/libtrace-db.so

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

CXXFLAGS = -O3 -Wall -I$(TRACE_DB_HEADERS_DIR) -I$(BOOST_INCLUDE_DIR) -fPIC -std=c++11

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

.PHONY: libtrace-db.so
libtrace-db.so: $(LIBTRACEDB_SO)


## Testing the trace database
BIN_TEST_DIR=test/cffi-interface/trace-db
BIN_TESTS=unit-test
TEST_ARTIFACTS=$(BIN_TEST_DIR)/unit-test \
               $(BIN_TEST_DIR)/sample

$(BIN_TEST_DIR)/sample.o: $(HEADERS) $(BOOST_SERIALIZATION)

$(BIN_TEST_DIR)/sample: $(BIN_TEST_DIR)/sample.o
	$(CXX) $(CXXFLAGS) -o $(BIN_TEST_DIR)/sample $(BIN_TEST_DIR)/sample.o -pthread -L $(BOOST_LIB_DIR) -lboost_iostreams -lboost_system -lboost_serialization

.PHONY: sample
sample: $(BIN_TEST_DIR)/sample

$(BIN_TEST_DIR)/unit-test.o: $(HEADERS) $(BOOST_SERIALIZATION)

$(BIN_TEST_DIR)/unit-test: $(BIN_TEST_DIR)/unit-test.o
	$(CXX) $(CXXFLAGS) -o $(BIN_TEST_DIR)/unit-test $(BIN_TEST_DIR)/unit-test.o -pthread -L $(BOOST_LIB_DIR) -lboost_iostreams -lboost_system -lboost_serialization

.PHONY: unit-test
unit-test: $(BIN_TEST_DIR)/unit-test


## Docs
DOC_PACKAGES=                                      \
    trace-db/core                                  \
    trace-db/trace-db                              \
    trace-db/sexp-trace-db                         \
    trace-db/binary-trace-db                       \
    trace-db/traceable                             \
    trace-db/instrumentation/clang-instrument


## Cleaning the trace database
trace-db-clean:
	rm -f $(LIBTRACEDB_SO)
	find . -iname *.o -delete


## Include default cl.mk
include .cl-make/cl.mk
