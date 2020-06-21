ARG DOCKER_REGISTRY
FROM $DOCKER_REGISTRY/synthesis/sel
COPY . /root/quicklisp/local-projects/trace-db
WORKDIR /root/quicklisp/local-projects/trace-db
CMD /bin/bash
