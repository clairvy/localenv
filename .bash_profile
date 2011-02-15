#!/usr/bin/env bash

if [[ -x /usr/bin/zsh ]]; then
    export SHELL=/usr/bin/zsh
    exec $SHELL
fi
