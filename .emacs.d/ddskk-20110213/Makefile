# Makefile: makefile for SKK.
#
# Maintainer: SKK Development Team <skk@ring.gr.jp>
# Version: $Id: Makefile,v 1.79 2011/01/03 06:37:05 skk-cvs Exp $
# Last Modified: $Date: 2011/01/03 06:37:05 $


VERSION = 14.2.50

BZIP2     = bzip2 -9
DATE	  = date
EMACS	  = emacs
ETAGS	  = etags
FLAGS     = -batch -q -no-site-file -l SKK-MK
GZIP      = gzip -9
MD5	  = md5
RM	  = /bin/rm -f
SNAPBASE  = ddskk-`$(DATE) '+%Y%m%d'`
TAR	  = gtar
XEMACS	  = xemacs
RUBY      = ruby
PYTHON    = python
SKK2CDB   = ./etc/skk2cdb.py -f
CDB_SOURCE = ./dic/SKK-JISYO.L
#CDB_TARGET = ./dic/$(notdir $(CDB_SOURCE)).cdb
CDB_TARGET = ./dic/`basename $(CDB_SOURCE)`.cdb
SKK_DEFAULT_JISYO =
set_jisyo =

elc:
	$(EMACS) $(FLAGS) -f SKK-MK-compile

package:
	$(XEMACS) $(FLAGS) -f SKK-MK-compile-package

info:
	$(EMACS) $(FLAGS) -f SKK-MK-compile-info

install:
	$(EMACS) $(FLAGS) -f SKK-MK-install

install-elc:
	$(EMACS) $(FLAGS) -f SKK-MK-install-elc

install-info:
	$(EMACS) $(FLAGS) -f SKK-MK-install-info

install-package:
	$(XEMACS) $(FLAGS) -f SKK-MK-install-package


what-where:
	$(EMACS) $(FLAGS) -f SKK-MK-what-where

what-where-package:
	$(XEMACS) $(FLAGS) -f SKK-MK-what-where-package

cdb:
	$(PYTHON) $(SKK2CDB) $(CDB_TARGET) $(CDB_SOURCE)

TAGS:
	$(ETAGS) `find . -name '*.el'`
clean:
	-$(RM) leim-list.el skk-autoloads.el skk-setup.el *.elc experimental/*.elc \
	auto-autoloads.el custom-load.el \
	./doc/skk.info* `find . -name '*~'` `find . -name '.*~'` `find . -name '.#*'`

tar: clean
	cd .. ;\
	$(RM) ddskk-11.{1,2,3} ddskk-$(VERSION) ddskk-snapshot ddskk$(VERSION).tar.gz ddskk$(VERSION).tar.bz2 ddskk-$(VERSION).tar.gz ddskk-$(VERSION).tar.bz2 ;\
	$(RM) ddskk-$(VERSION) ;\
	ln -sf main ddskk-$(VERSION) ;\
	$(TAR) -cvpf ddskk-$(VERSION).tar --exclude-from=ddskk-$(VERSION)/skk.ex --dereference ddskk-$(VERSION) ;\
	$(BZIP2) -cf ddskk-$(VERSION).tar > ddskk-$(VERSION).tar.bz2 ;\
	$(GZIP) -cf ddskk-$(VERSION).tar > ddskk-$(VERSION).tar.gz ;\
	$(RM) ddskk-$(VERSION).tar ;\
	$(RM) ddskk-$(VERSION) ;\
	$(MD5) ddskk-$(VERSION).tar.bz2 >ddskk-$(VERSION).tar.bz2.md5 ;\
	$(MD5) ddskk-$(VERSION).tar.gz >ddskk-$(VERSION).tar.gz.md5

snapshot: clean
	cd .. ;\
	$(RM) ddskk-11.{1,2,3} ddskk-$(VERSION) ddskk-snapshot $(SNAPBASE).tar.gz $(SNAPBASE).tar.bz2 ;\
	$(RM) $(SNAPBASE) ;\
	ln -sf main $(SNAPBASE) ;\
	$(TAR) -cvpf $(SNAPBASE).tar --exclude-from=$(SNAPBASE)/skk.ex --dereference $(SNAPBASE);\
	$(GZIP) -cf $(SNAPBASE).tar > $(SNAPBASE).tar.gz ;\
	$(RM) $(SNAPBASE).tar ;\
	$(RM) $(SNAPBASE) ;\
	$(MD5) $(SNAPBASE).tar.gz >$(SNAPBASE).tar.gz.md5

# end of Makefile.
