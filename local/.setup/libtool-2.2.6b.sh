#!/bin/sh

src=../modules/libtool-2.2.6b
dirs='bin lib include'
for d in $dirs; do
  ( cd $d && ls -d ../$src/$d/* | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
done
