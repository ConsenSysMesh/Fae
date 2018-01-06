FROM ubuntu
ENV STACK_ROOT=/usr/lib/stack\
    FAE_HOME=/var/lib/fae
ADD known_hosts id_rsa /root/.ssh/
ADD faeServer.* /etc/
ADD build.sh /
RUN chmod +x /build.sh && /build.sh && rm /build.sh
WORKDIR $FAE_HOME/run
ENTRYPOINT ["/usr/bin/faeServer.sh"]
EXPOSE 27182
