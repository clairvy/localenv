#!/bin/sh

src=../modules/eggx-0.84
dirs='bin include lib'

for d in $dirs; do
  ( cd $d && /bin/ls ../$src/$d/* | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
done
