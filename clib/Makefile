# Makefile for clib

include ../make.inc

OBJS = \
customize_signals.o \
stack.o \
c_mkdir.o \
copy.o \
cptimer.o \
eval_infix.o \
indici.o \
md5.o \
md5_from_file.o \
memstat.o \
ptrace.o \
sockets.o \
qmmm_aux.o \
qsort.o

OBJSCXX = \
dtrplugin.o

all : clib.a

clib.a :

ifeq ($(findstring -D__DTR,$(DFLAGS)),-D__DTR)
clib.a : $(OBJS) $(OBJSCXX)
	$(AR) $(ARFLAGS) $@ $?
	$(RANLIB) $@
else
clib.a : $(OBJS)
	$(AR) $(ARFLAGS) $@ $?
	$(RANLIB) $@
endif

source :
	co -l $(OBJS:.o=.c)

dtrplugin.o: dtrplugin.cxx
	$(CXX) $(CXXFLAGS) $(SHLDFLAGS) $(IFLAGS) -c dtrplugin.cxx

clean :
	- rm -f clib.a *.o *.mod *.i core*

include make.depend
