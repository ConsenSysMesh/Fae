#!/bin/bash
if [[ $# -eq 0 ]]
then echo >&2 "Must supply a transaction spec file name"; exit 1
fi

if [[ -n $FAE_REPO ]]
then container=$FAE_REPO/posttx
else container=posttx
fi

cd $(dirname $1)
txfile=$(basename $1)
shift

docker run \
  --rm \
  --network host \
  --mount type=bind,src=$PWD,dst=/txs/,readonly \
  $container $txfile $@

