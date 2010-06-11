#!/bin/sh

src="../modules/hadoop-0.20.2"
wrapper="../$src/bin/wrapper.sh"

( cd bin && ls -d ../$src/bin/{hadoop,start-all.sh,stop-all.sh} | xargs -I {} echo 'rm `basename {}`; ln -s '"$wrapper"' `basename {}`' | sh )
