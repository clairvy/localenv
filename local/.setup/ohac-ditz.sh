#!/bin/sh

src=../modules/ohac-ditz
( cd bin && ls -d ../$src/bin/* | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
( cd share/man/man1 && ls -d ../../../$src/share/man/man1/* | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
