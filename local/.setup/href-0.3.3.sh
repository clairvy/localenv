#!/bin/sh

root=$HOME/modules/href-0.3.3
( cd bin && ( for f in `ls $root/bin/* | egrep -v wrapper.sh`; do rm `basename $f`; ln -s $root/bin/wrapper.sh `basename $f`; done ) )
