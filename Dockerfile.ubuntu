FROM ubuntu:16.04

RUN mkdir -p /gt/

RUN apt-get -y update && \
    apt-get -y install make build-essential wget

WORKDIR /gt

RUN wget https://dl.bintray.com/boostorg/release/1.67.0/source/boost_1_67_0.tar.bz2 && \
    tar --bzip2 -xf boost_1_67_0.tar.bz2 && \
    cd /gt/boost_1_67_0/ && \
    ./bootstrap.sh && \
    ./b2 install && \
    ldconfig

COPY . /gt/trace-db

CMD /bin/bash
