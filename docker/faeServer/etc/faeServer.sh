#!/bin/bash
imgName=teamfae/faeserver
if [[ -n $FAE_VERSION ]]
then imgName=$imgName:$FAE_VERSION
fi

docker run -it --network host --rm $imgName $@
