#!/bin/bash
imgName=teamfae/faeserver
if [[ -n $FAE_VERSION ]]
then imgName=$imgName:$FAE_VERSION
fi

docker run -p 27182:27182 --network host --rm $imgName
