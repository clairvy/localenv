update

upgrade

install zsh 2>&1 | egrep -v '^Warning: ' || true
install git 2>&1 | egrep -v '^Warning: ' || true
install lv 2>&1 | egrep -v '^Warning: ' || true
install rlwrap 2>&1 | egrep -v '^Warning: ' || true
install emacs --cocoa 2>&1 | egrep -v '^Warning: ' || true
linkapps emacs

#uninstall --force brew-cask
tap caskroom/cask
tap homebrew/versions

cask install dockertoolbox 2>&1 | egrep -v ' already installed.' || true

cask install alfred 2>&1 | egrep -v ' already installed.' || true
cask install karabiner 2>&1 | egrep -v ' already installed.' || true
cask install iterm2 2>&1 | egrep -v ' already installed.' || true
cask install evernote 2>&1 | egrep -v ' already installed.' || true
cask install bettertouchtool 2>&1 | egrep -v ' already installed.' || true
cask install totalfinder 2>&1 | egrep -v ' already installed.' || true
cask install aquaskk 2>&1 | egrep -v ' already installed.' || true
cask install limechat 2>&1 | egrep -v ' already installed.' || true

cask install eclipse-java 2>&1 | egrep -v ' already installed.' || true
cask install intellij-idea-ce 2>&1 | egrep -v ' already installed.'

install homebrew/completions/tmuxinator-completion 2>&1 | egrep -v ' already installed' || true

cleanup
