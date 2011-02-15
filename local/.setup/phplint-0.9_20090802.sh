#!/bin/sh

src=../modules/phplint-0.9_20090802
( cd bin && ls -d ../$src/bin/* | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
