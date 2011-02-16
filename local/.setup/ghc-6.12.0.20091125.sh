#!/bin/sh

root=$HOME/modules/ghc-6.12.0.20091125
( cd bin && ( for f in $root/bin/*; do rm `basename $f`; ln -s $f . ; done ) )
