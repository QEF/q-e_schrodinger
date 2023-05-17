#
#===============================
# deviceXlib package
#===============================
#
-include ../make.inc

DIR=src_gener

#
# main targets
#
all: memcpy auxfunc fbuff

memcpy auxfunc fbuff:
	if [ -x generate_$@.py ] ; then ./generate_$@.py ; \
	   mv *.f90 *.F ../src ; fi

# clean
clean:
	 - /bin/rm -rf *.f90 *.F __*__ 
distclean:   clean


# dependencies
include make.depend

