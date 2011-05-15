#!/bin/sh

src=../modules/git-1.7.4.5
#( cd bin && ls -d ../$src/bin/* | xargs -I {} echo 'rm `basename {}`; ln -s `dirname {}`/wrapper.sh `basename {}`' )
( cd bin && ls -d ../$src/bin/* | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
