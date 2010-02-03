#!/bin/sh

# http://rubyforge.org/pipermail/mongrel-users/2009-November/005853.html
# for ruby 1.9
gem install mongrel --source http://gems.rubyinstaller.org
gem install rascut

# see also
# http://gist.github.com/293283
# change mname.match -> mname.to_s.match (tr Symbol to String)

