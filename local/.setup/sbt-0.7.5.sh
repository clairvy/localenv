#!/bin/sh

src=../modules/sbt-0.7.5
( cd bin && ls -d ../$src/bin/sbt | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
