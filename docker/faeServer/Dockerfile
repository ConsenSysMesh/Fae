FROM scratch
ENV FAE_HOME=/var/lib/fae\
    GHC_PACKAGE_PATH=/lib/ghc-pkgdb\
    GHC_LIBDIR=/lib\
    TMPDIR=/tmp\
    LD_LIBRARY_PATH=/lib:/lib64
WORKDIR $TMPDIR
WORKDIR $FAE_HOME
ENTRYPOINT ["/bin/faeServer"]
EXPOSE 27182
ADD . /
