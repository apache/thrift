#!/bin/sh

if [ -z $BASE_PKG ]; then
    BASE_PKG=`ghc-pkg --simple-output list base-3* | sed -e "s/.*\(base-3\(.[0-9]\){3}\).*/\1/"`
fi

if [ -z $BASE ]; then
    BASE=../..
fi

ghci -fglasgow-exts -package $BASE_PKG -hide-package syb -i$BASE/lib/hs/src -i$BASE/test/hs/gen-hs Client.hs
