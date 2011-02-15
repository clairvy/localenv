#!/bin/sh

src=../modules/wget-1.12
( cd bin && ls -d ../$src/bin/* | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
