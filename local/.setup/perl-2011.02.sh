#!/bin/sh

src=../s/app/rakudo-star/rakudo-star-2011.02
( cd bin && ls -d ../$src/install/bin/* | egrep 'perl6|ufo' | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
