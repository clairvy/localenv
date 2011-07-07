#!/bin/sh

root=../modules/rencon
( cd bin && ls -d ../$root/bin/* | egrep -v wrapper.sh | xargs -I {} echo 'rm `basename {}`; ln -s `dirname {}`/wrapper.sh `basename {}`' | sh )
