#!/bin/sh

src=../modules/php-5.2.6
( cd bin && ln -s ../$src/bin/php php )
( cd bin && ln -s ../$src/bin/pear pear )
( cd bin && ln -s ../$src/bin/pearproj pearproj )
