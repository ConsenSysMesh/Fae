FROM scratch
ENV FAE_HOME=/var/lib/fae\
    GHC_PACKAGE_PATH=/lib/ghc-pkgdb\
    GHC_LIBDIR=/lib\
    TMPDIR=/tmp
WORKDIR $TMPDIR
WORKDIR $FAE_HOME
ENTRYPOINT ["/bin/faeServer"]
EXPOSE 27182
ADD . /
