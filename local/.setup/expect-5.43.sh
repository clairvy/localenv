#!/bin/sh

src=../modules/expect-5.43
( cd bin && ls -d ../$src/bin/* | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
