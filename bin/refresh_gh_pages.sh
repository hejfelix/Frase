#!/bin/bash
TARGET=web/target/scala-2.12/scalajs-bundler/main
mkdir -p deploy
cp -v $TARGET/frase-web-fastopt-library.js ./deploy
cp -v $TARGET/launcher-library.js ./deploy
cp -v web/public/index-fastopt.html ./deploy
mv -v ./deploy/index-fastopt.html ./deploy/index.html