#!/bin/sh

root=$HOME/modules/ghc-6.10.4
( cd bin && ( for f in $root/bin/*; do rm `basename $f`; ln -s $f . ; done ) )
