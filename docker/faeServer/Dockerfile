FROM ubuntu
ARG GIT_BRANCH=master
ENV STACK_ROOT=/usr/lib/stack\
    FAE_HOME=/var/lib/fae
ADD . /
RUN chmod +x /build.sh && /build.sh && rm /build.sh
ENTRYPOINT ["/usr/bin/faeServer.sh"]
EXPOSE 27182
