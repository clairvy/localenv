#!/bin/sh

root=../modules/jdk-1.6.0_21
( cd bin && ls -d ../$root/bin/* | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
