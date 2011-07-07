#!/bin/sh

# ./configure --prefix=$HOME/modules/ruby-1.9.1-p378 2> c.err | tee c.log
# make 2> m.err | tee m.log
# make test
# make install

root=../modules/ruby-1.8.7-p334
( cd bin && ls -d ../$root/bin/* | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
