.PHONY: all clean distclean

all:
	make -f innative.mk
	make -f innative-env.mk
	make -f innative-test.mk
	make -f innative-cmd.mk

clean:
	make clean -f innative.mk
	make clean -f innative-env.mk
	make clean -f innative-test.mk
	make clean -f innative-cmd.mk

dist: all distclean
	tar -czf innative-posix.tar.gz *

distclean:
	make distclean -f innative.mk
	make distclean -f innative-env.mk
	make distclean -f innative-test.mk
	make distclean -f innative-cmd.mk

debug:
	make debug -f innative.mk
	make debug -f innative-env.mk
	make debug -f innative-test.mk
	make debug -f innative-cmd.mk

install: all
	make install -f innative.mk
  
uninstall:
	make uninstall -f innative.mk