#!/bin/sh

src=../modules/xsbt
( cd bin && ls -d ../$src/target/xsbt | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
