#!/bin/bash
imgName=fae:faeServer
if [[ -n $FAE_REPO ]]
then imgName=$FAE_REPO/$imgName
fi

docker run -p 27182:27182 --network host --rm $imgName
