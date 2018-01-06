REM @ECHO OFF

IF %* == "" (
  echo "Must supply a transaction spec file name" >&2
  exit /b 1
)

SET fulltxfile=%~f1

IF defined FAE_REPO (
  SET container=%FAE_REPO%/posttx
) ELSE (
  SET container=posttx
)

docker run ^
  --rm ^
  --network host ^
  --mount type=bind,src=%fulltxfile%,dst=/txs/%1,readonly ^
  %container% %*

