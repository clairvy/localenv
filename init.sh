#!/bin/sh

for f in .*; do
  if [ x"$f" == x'.' -o x"$f" == x'..' ]; then
    continue
  fi
  if [ x"$f" == x'.gitmodules' -o x"$f" == x'.git' -o x"$f" == x'.gitignore' ]; then
    continue
  fi
  if [ x"$f" == x'.hosts' ]; then
    continue
  fi
  if [ x"$f" == x'.ssh' ]; then
    continue
  fi
  pushd ~
#pwd
  if [ -f $f ]; then
#    echo mv $f $f.org
    mv $f $f.org
  fi
#  echo ln -s s/localenv/$f .
  ln -s s/localenv/$f .
  popd
#pwd
done
