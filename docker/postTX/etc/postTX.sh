#!/bin/bash
if [[ $# -eq 0 ]]
then echo >&2 "Must supply a transaction spec file name"; exit 1
fi

imgName=fae:postTX
if [[ -n $FAE_REPO ]]
then imgName=$FAE_REPO/$imgName
fi

envList=""
while true; do
  case $1 in
    -e|--env-list|--env-file)
      envList="$envList $1 $2"
      shift 2
      ;;
    *) break ;;
  esac
done

cd $(dirname $1)
txfile=$(basename $1)
shift

docker run \
  --rm \
  --network host \
  --mount type=bind,src=$PWD,dst=/txs/,readonly \
  $envList $imgName $txfile $@

