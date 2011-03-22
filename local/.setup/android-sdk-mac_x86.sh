#!/bin/sh

root=../modules/android-sdk-mac_x86
( cd bin && ls -d ../$root/tools/* | xargs -I {} echo 'rm `basename {}`; ln -s {} `basename {}`' | sh )
rm android; ln -s $root android
