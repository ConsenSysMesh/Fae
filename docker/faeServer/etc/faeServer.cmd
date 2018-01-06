@ECHO off

SET imgName=fae:faeServer
IF defined FAE_REPO (
  SET container=%FAE_REPO%/%imgName%
)
docker run -p 27182:27182 --network host --rm %imgName%
