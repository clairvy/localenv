update

upgrade

install zsh 2>&1 | egrep -v '^Warning: ' || true
install git 2>&1 | egrep -v '^Warning: ' || true
install lv 2>&1 | egrep -v '^Warning: ' || true
install rlwrap 2>&1 | egrep -v '^Warning: ' || true
install emacs 2>&1 | egrep -v '^Warning: ' || true

tap 'caskroom/cask' 2>&1 | egrep -v '^Warning: ' || true
install brew-cask 2>&1 | egrep -v '^Warning: ' || true

cask install boot2docker 2>&1 | egrep -v ' already installed.' || true

cleanup
