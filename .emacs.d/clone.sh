#!/bin/sh
rmdir egg-snapshot
git clone git://github.com/bogolisk/egg.git egg-snapshot
rmdir gist-snapshot
git clone git://github.com/defunkt/gist.el.git gist-snapshot
rmdir perl-completion
git clone git://github.com/imakado/perl-completion.git
rmdir cperl-mode
git clone git://github.com/jrockway/cperl-mode.git
rmdir cperl-mode
git clone git://github.com/jrockway/cperl-mode.git
rmdir perl-completion
git clone git://github.com/imakado/perl-completion.git
# id:IMAKADO++
rmdir cperl-calculate-indent
git clone git://gist.github.com/145957.git cperl-calculate-indent
( cd cperl-calculate-indent && git mv cperl-calculate-indent.txt cperl-calculate-indent.el )
rmdir simple-hatena-mode
git svn clone http://svn.coderepos.org/share/lang/elisp/simple-hatena-mode/ -T trunk -b branch -t tags simple-hatena-mode 
