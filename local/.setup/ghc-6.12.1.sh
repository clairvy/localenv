#!/bin/sh

root=../modules/ghc-6.12.1
( cd bin && ls -d ../$root/bin/* | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
