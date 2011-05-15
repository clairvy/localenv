#!/bin/sh

root=../modules/curl-7.21.5
( cd bin && ls -d ../$root/bin/* | egrep -v pkgconfig | xargs -I {} echo 'rm `basename {}`; ln -s `dirname {}`/wrapper.sh `basename {}`' | sh )
dirs='include lib'
for d in $dirs; do
  ( cd $d && ls -d ../$root/$d/* | egrep -v pkgconfig | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
done
