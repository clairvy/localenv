#!/bin/sh

src=../modules/android-plugin/script/
( cd bin && ls -d ../$src/* | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
