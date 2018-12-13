#!/bin/bash
imgName=teamfae/faeserver
if [[ -n $FAE_VERSION ]]
then imgName=$imgName:$FAE_VERSION
fi

faeDir=${FAE_HOME:-~/fae}
[[ -d $faeDir ]] || mkdir -p $faeDir

FAE_UID=$(id -u)
FAE_GID=$(id -g)

docker run -it --rm \
  --user $FAE_UID:$FAE_GID \
  --network host \
  --mount type=bind,src=$faeDir,dst=/fae/ \
  $imgName $@
