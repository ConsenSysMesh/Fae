ARG LDSO
FROM scratch
COPY postTX /bin/
COPY *.so* /lib/
COPY $LDSO /lib64/
WORKDIR /txs
COPY HelloWorld* ./
ENTRYPOINT ["/bin/postTX"]
