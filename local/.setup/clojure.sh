#!/bin/sh

root=../s/app/clojure
( cd bin && ls -d ../$root/clj | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
