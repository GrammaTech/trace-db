FROM ubuntu:16.04

RUN mkdir -p /gt/

RUN apt-get -y update && \
    apt-get -y install make build-essential

COPY . /gt/trace-db

WORKDIR /gt
CMD /bin/bash
