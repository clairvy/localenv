update

upgrade

install zsh 2>&1 | egrep -v '^Warning: ' || true
install git 2>&1 | egrep -v '^Warning: ' || true
install lv 2>&1 | egrep -v '^Warning: ' || true
install rlwrap 2>&1 | egrep -v '^Warning: ' || true
install emacs --cocoa 2>&1 | egrep -v '^Warning: ' || true
linkapps emacs
install macvim
install tig

#uninstall --force brew-cask
tap caskroom/cask
tap homebrew/versions

# tmux-xpane
tap greymd/tools
install tmux-xpanes

cask install alfred 2>&1 | egrep -v ' already installed.' || true
#cask install karabiner 2>&1 | egrep -v ' already installed.' || true
cask install karabiner-elements 2>&1 | egrep -v ' already installed.' || true
cask install iterm2 2>&1 | egrep -v ' already installed.' || true
cask install evernote 2>&1 | egrep -v ' already installed.' || true
cask install bettertouchtool 2>&1 | egrep -v ' already installed.' || true
cask install aquaskk 2>&1 | egrep -v ' already installed.' || true
cask install limechat 2>&1 | egrep -v ' already installed.' || true
cask install yujitach-menumeters 2>&1 | egrep -v ' already installed.' || true

cleanup
