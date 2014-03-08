update

upgrade

install zsh 2>&1 | egrep -v '^Warning: ' || true
install git 2>&1 | egrep -v '^Warning: ' || true
install lv 2>&1 | egrep -v '^Warning: ' || true
install rlwrap 2>&1 | egrep -v '^Warning: ' || true
install emacs 2>&1 | egrep -v '^Warning: ' || true

tap 'phinze/cask' 2>&1 | egrep -v '^Warning: ' || true
install brew-cask 2>&1 | egrep -v '^Warning: ' || true

cask install vagrant 2>&1 | egrep -v '^Warning: ' || true
cask install virtualbox 2>&1 | egrep -v '^Warning: ' || true

cleanup
