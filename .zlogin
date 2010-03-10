#!/usr/bin/env zsh

if [[ -x $HOME/local/bin/zsh ]]; then
  local shell=$HOME/local/bin/zsh
  exec $shell
fi
