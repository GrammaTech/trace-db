FROM ubuntu:20.04

RUN export DEBIAN_FRONTEND=noninteractive
RUN ln -fs /usr/share/zoneinfo/America/New_York /etc/localtime
RUN apt-get -y update && \
    apt-get -y install make build-essential libboost-iostreams-dev libboost-system-dev libboost-serialization-dev

COPY . /root/quicklisp/local-projects/trace-db
WORKDIR /root/quicklisp/local-projects/trace-db

CMD /bin/bash
