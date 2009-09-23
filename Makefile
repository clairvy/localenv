# -*- mode: makefile; coding: utf-8-unix; -*-

ARCHIVE = tar cf

OBJECTS = .emacs .emacs.d .vimrc .zshrc

TAR_FILE = ../localenv.tar

NCFTPPUT = ncftpput

archive: $(TAR_FILE)

$(TAR_FILE):
	$(ARCHIVE) $(TAR_FILE) $(OBJECTS)

clean:
	$(RM) $(RMF) $(TAR_FILE)
