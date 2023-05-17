#
#===============================
# deviceXlib package
#===============================
#
-include ../make.inc

DIR=src
LIBOBJS_LOC= $(LIBOBJS)

OBJS= \
deviceXlib_mod.o \
device_memcpy.o \
device_memcpy_mod.o \
device_auxfunc.o \
device_auxfunc_mod.o \
device_fbuff.o \
device_fbuff_mod.o \
timer.o \
timer_mod.o

#
# main targets
#
all: libdevXlib.a

test: test_memcpy.x test_memcpy_async.x test_fbuff.x

fortcuda:
	$(MAKE) -C FortCuda


test_memcpy.x: test_memcpy.o $(OBJS)
	$(LD) $(LDFLAGS) -o $@ test_memcpy.o \
	       $(OBJS) $(LIBOBJS_LOC) $(LIBS)
	- ( cd ../bin ; ln -sf ../src/$@ . )

test_memcpy_async.x: test_memcpy_async.o $(OBJS)
	$(LD) $(LDFLAGS) -o $@ test_memcpy_async.o \
	       $(OBJS) $(LIBOBJS_LOC) $(LIBS)
	- ( cd ../bin ; ln -sf ../src/$@ . )

test_fbuff.x: tester.o test_fbuff.o $(OBJS)
	$(LD) $(LDFLAGS) -o $@ tester.o test_fbuff.o \
	       $(OBJS) $(LIBOBJS_LOC) $(LIBS)
	- ( cd ../bin ; ln -sf ../src/$@ . )

libdevXlib.a : $(OBJS)
	$(AR) $(ARFLAGS) $@ $?
	$(RANLIB) $@

# clean
clean:
	 - /bin/rm -rf *.o *.a *core *.F90 work.pc work.pcl *.mod *.x
	 - cd FortCuda && /bin/rm -rf *.o *.a *core work.pc work.pcl *.mod *.x
distclean:   clean


# dependencies
include make.depend

