#!/bin/sh

root=../modules/openssl-1.0.0d
for d in bin include lib; do
  ( cd $d && ls -d ../$root/$d/* |egrep -v pkgconfig| xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
done
ln -s $root/ssl .
