FROM ubuntu:16.04

RUN mkdir -p /gt/

RUN apt-get -y update && \
    apt-get -y install git make build-essential autoconf libtool

ENV GT_ROOT=/gt \
    USER=docker \
    HOSTNAME=docker

RUN mkdir -p /gt/trace-db && \
    GIT_SSH_COMMAND="ssh -o StrictHostKeyChecking=no" git clone https://git.grammatech.com/research/trace-db.git /gt/trace-db && \
    cd /gt/trace-db && \
    git checkout CI_COMMIT_SHA

WORKDIR /gt
CMD /bin/bash
