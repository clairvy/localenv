update

upgrade

install zsh 2>&1 | egrep -v '^Warning: ' || true
install git 2>&1 | egrep -v '^Warning: ' || true
install lv 2>&1 | egrep -v '^Warning: ' || true
install rlwrap 2>&1 | egrep -v '^Warning: ' || true
install emacs 2>&1 | egrep -v '^Warning: ' || true
install peco 2>&1 | egrep -v '^Warning: ' || true

install caskroom/cask/brew-cask 2>&1 | egrep -v '^Warning: ' || true

#cask install boot2docker 2>&1 | egrep -v ' already installed.' || true
#install fig 2>&1 | egrep -v '^Warning: ' || true
cask install dockertoolbox 2>&1 | egrep -v ' already installed.' || true

cask install alfred 2>&1 | egrep -v ' already installed.' || true
cask install karabiner 2>&1 | egrep -v ' already installed.' || true

cask install bettertouchtool 2>&1 | egrep -v ' already installed.' || true
cask install atom 2>&1 | egrep -v ' already installed.' || true

install homebrew/completions/tmuxinator-completion 2>&1 | egrep -v ' already installed' || true

cleanup
