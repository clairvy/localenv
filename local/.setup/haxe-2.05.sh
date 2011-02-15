#!/bin/sh

src=~/modules/haxe-2.05-linux
wrapper=wrapper.sh
( cd bin && ls -d $src/haxe* | xargs -I {} echo 'rm `basename {}`; ln -s `dirname {}`/'$wrapper' `basename {}`' | sh )
