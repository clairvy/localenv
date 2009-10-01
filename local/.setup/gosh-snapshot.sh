#!/bin/sh

src=../modules/gauche-snapshot

( cd bin && ln -s ../$src/bin/g* . )
