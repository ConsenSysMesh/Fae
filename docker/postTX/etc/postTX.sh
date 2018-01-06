#!/bin/bash
if [[ $# -eq 0 ]]
then echo >&2 "Must supply a transaction spec file name"; exit 1
fi

imgName=fae:postTX
if [[ -n $FAE_REPO ]]
then imgName=$FAE_REPO/$imgName
fi

cd $(dirname $1)
txfile=$(basename $1)
shift

docker run \
  --rm \
  --network host \
  --mount type=bind,src=$PWD,dst=/txs/,readonly \
  $imgName $txfile $@

