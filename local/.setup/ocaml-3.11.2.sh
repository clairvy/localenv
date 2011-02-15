#!/bin/sh

src=../modules/ocaml-3.11.2
( cd bin && ls -d ../$src/bin/* | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
