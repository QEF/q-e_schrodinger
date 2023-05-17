##
## Makefile
##  
## Made by David Car, modified by Pietro Bonfa'
##
## Started on  Sat Oct 17 16:17:38 2009 David Car
## Last update Sat Oct 17 16:17:38 2009 David Car
## 
include ../../make.inc

.SUFFIXES: .F90 .o

MOD_DIR=.
LIB_DIR=.

DEPENDENCIES=Make.depend

#FC ?= gfortran
FFLAGS += $(FFLAGS_LINELEN)
INCDIR ?= /usr/local/cuda/include

OUTPUT_CUDA=$(LIB_DIR)/libFortCUDA.a
OUTPUT_OPENCL=$(LIB_DIR)/libFortCL.a
OUTPUT_GPU=$(LIB_DIR)/libFortGPU.a
default: strip_comments $(OUTPUT_CUDA) restore_comments
all: $(OUTPUT_CUDA) $(OUTPUT_OPENCL) $(OUTPUT_GPU)
########################################################################
##
## PRIMARY TARGETS
##
########################################################################
$(OUTPUT_CUDA): CUDA.o
	$(AR) -rs $@ $^
$(OUTPUT_OPENCL): OpenCL.o
	$(AR) -rs $@ $^
$(OUTPUT_GPU): CUDA.o OpenCL.o
	$(AR) -rs $@ $^

autoparse:
	python FortCUDA_File_Parser.py -E --log-file=log -m --save-pp \
		-I/usr/local/cuda/include

strip_comments:
	tar -cf .fortranfiles.tar *.F90
	sed -i 's:!.*$$::g' *.F90

restore_comments:
	tar -xf .fortranfiles.tar


########################################################################
##
## IMPLICIT RULES
##
########################################################################
.f90.o:
	$(MPIF90) $(COMPATIBILITY) -c $(FFLAGS) -I$(MOD_DIR) $<
.F90.o:
	$(MPIF90) $(COMPATIBILITY) -c $(FFLAGS) -I$(MOD_DIR) $<

########################################################################
##
## CLEANING TARGETS
##
########################################################################
clean:
	-rm *.o *.mod
cleaner: clean
cleanest: cleaner

-include $(DEPENDENCIES)
