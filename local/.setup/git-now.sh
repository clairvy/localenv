#!/bin/sh

root=../modules/git-now
( cd bin && ls -d ../$root/git-now | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
