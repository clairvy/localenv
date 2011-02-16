#!/bin/sh

# ruby setup.rb --prefix=$HOME/modules/rubygems-1.3.5

root=../modules/rubygems-1.3.5
( cd bin && ls -d ../$root/bin/* | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
