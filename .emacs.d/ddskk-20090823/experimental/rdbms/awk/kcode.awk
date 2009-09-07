# kcode.awk --
#
# Copyright (C) 1998 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
#
# Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
# Created: Sep 1, 1998
# Last Modified: $Date: 2001/02/03 00:22:59 $
# Version: $Id: kcode.awk,v 1.2 2001/02/03 00:22:59 minakaji Exp $
#
# This file is not part of SKK yet.
#
# SKK is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either versions 2, or (at your option)
# any later version.
#
# SKK is distributed in the hope that it will be useful
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with SKK, see the file COPYING.  If not, write to the Free
# Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
# MA 02111-1307, USA.
#
# Commentary:
# The format of output file is (delimiter is TAB);
#
#        kanji	JIScode	Unicode
#
# and how to convert;
#
#        % gawk -f kcode.awk kanjidic > temp5.txt
#
# Code

/^[^#]/{
  if (match($0, /U[0-9a-z]+/) != 0) {
    unicode = substr($0, RSTART + 1, RLENGTH - 1);
  } else
    unicode = "";
  printf("%s\t%s\t%s\n",$1, $2, unicode);
}
# end of kcode.awk
