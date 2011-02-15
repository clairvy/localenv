#!/bin/sh

root=../modules/scala-2.8.0.final
( cd bin && ls -d ../$root/bin/* | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
