#!/bin/sh

root=../s/app/star/rakudo-star-2011.02/install
( cd bin && ls -d ../$root/bin/perl6 | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
( cd bin && ls -d ../$root/bin/ufo | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
