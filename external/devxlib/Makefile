#
#===============================
# deviceXlib package
#===============================
#
MAKE=make

#
# manual
#
default: 
	@echo 
	@echo  "  *** deviceXlib *** "
	@echo 
	@echo  "  to configure, type:  ./configure [<options>]"
	@echo  "  to compile,   type:  make <target>"
	@echo 
	@echo  "  Possible <target>'s are: "
	@echo 
	@echo  "     all                builds the library"
	@echo  "     test               all the executables testing the library"
	@echo  "     deps               update fortran90 dependencies"
	@echo  "     gen                generate Fortran sources"
	@echo  "     clean              remove executables and objects"
	@echo  "     clean_test         clean up the test examples"
	@echo  "     distclean          revert distribution to the original status"
	@echo 

#
# MAIN target
#
all: libsrc

deps:
	if test -x ./config/makedeps.sh ; then ./config/makedeps.sh ; fi
gen:
	if test -d src_generator ; then ( cd src_generator ; $(MAKE) ) ; fi

# 
# LIBS and MODULES
#
libextlibs: 
	if test -d extlibs ; then \
	( cd extlibs ; $(MAKE) ) ; fi

libsrc:
	if test -d src/ ; then \
	( cd src/ ; $(MAKE) ) ; fi

test: libextlibs libsrc
	if test -d src/ ; then \
	( cd src/ ; $(MAKE) test ) ; fi

#
# CLEAN UP
#
clean:
	if test -d extlibs ;   then ( cd extlibs;   $(MAKE) clean ) ; fi
	if test -d src ;       then ( cd src;       $(MAKE) clean ) ; fi
	- /bin/rm -rf *.dat *.mod
	- /bin/rm -rf ./bin/*x

distclean: clean
	if test -d extlibs ;   then ( cd extlibs;   $(MAKE) distclean ) ; fi
	- /bin/rm -rf make.inc config.log ./config/configure.msg \
		./config/config.log ./config/config.status \
		./config/*.lineno \
		./include/configure.h ./include/fft_defs.h \
	        ./include/c_defs.h    ./include/f_defs.h\
		*/dum1 */dum2 

