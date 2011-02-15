#!/bin/sh

src=../modules/emacs-20100422
( cd bin && ls -d ../$src/bin/* | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
