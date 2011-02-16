#!/bin/sh

src=../modules/sbt-launcher-0.7.4
( cd bin && ls -d ../$src/sbt | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
