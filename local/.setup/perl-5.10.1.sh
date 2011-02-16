#!/bin/sh

src=../modules/perl-5.10.1
( cd bin && ls -d ../$src/bin/perl5* | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
