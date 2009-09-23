#!/bin/sh

root=$HOME/modules/tscreen-0.4.9

for d in bin; do
  ( cd $d && ln -s $root/$d/* . )
done

if [ -e bin/screen ]; then
    rm bin/screen
fi
( cd bin && ln -s tscreen screen )
