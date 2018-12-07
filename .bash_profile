#!/usr/bin/env bash

if [[ -x /usr/bin/zsh ]]; then
    export SHELL=/usr/bin/zsh
    exec $SHELL
elif [[ -x /bin/zsh ]]; then
    export SHELL=/bin/zsh
    exec $SHELL
fi

export PATH="$HOME/.cargo/bin:$PATH"
