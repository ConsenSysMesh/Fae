#!/bin/bash
if [[ $# -eq 0 ]]
then echo >&2 "Must supply a transaction spec file name"; exit 1
fi

imgName=teamfae/posttx
if [[ -n $FAE_VERSION ]]
then imgName=$imgName:$FAE_VERSION
fi

faeDir=${FAE_HOME:-~/fae}
[[ -d $faeDir ]] || mkdir -p $faeDir

FAE_UID=$(id -u)
FAE_GID=$(id -g)

declare -a envlist
n=0
while true; do
  case $1 in
    -e|--env-list|--env-file)
      envlist[$n]=$1
      envlist[$n + 1]=$2
      let n+=2
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
  --interactive --tty \
  --network host \
  --user $FAE_UID:$FAE_GID \
  --mount type=bind,src=$PWD,dst=/txs/,readonly \
  --mount type=bind,src=$faeDir,dst=/fae/ \
  "${envlist[@]}" $imgName $txfile $@

