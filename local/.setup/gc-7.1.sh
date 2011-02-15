#!/bin/sh
dirs='include lib'
src='../modules/gc-7.1'
for d in $dirs; do
    ( cd $d && ls -d ../$src/$d/* | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
done
