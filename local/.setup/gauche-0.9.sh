#!/bin/sh

src=../modules/gauche-0.9

( cd bin && for f in ../$src/bin/g*; do rm `basename $f`; ln -s $f .; done )
