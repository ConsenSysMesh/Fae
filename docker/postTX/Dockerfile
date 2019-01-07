FROM scratch
ENV FAE_HOME=/fae\
    LD_LIBRARY_PATH=/lib:/lib64
WORKDIR $FAE_HOME
WORKDIR /txs
ENTRYPOINT ["/bin/postTX"]
ADD . /
