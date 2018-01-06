#!/bin/bash
if [[ -n $FAE_REPO ]]
then container=$FAE_REPO/posttx
else container=posttx
fi

docker run -p 27182:27182 --network host --rm $container
