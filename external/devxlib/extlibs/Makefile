#
#===============================
# deviceXlib package
#===============================
#
-include ../make.inc


#
# MAIN target
#
all: libblas liblapack

#
# blas
#
libblas:  libblas_$(BLAS_LIBS_SWITCH)

libblas_internal:
	 if test -d blas ;    then ( cd blas;    $(MAKE) ) ; fi

libblas_external:
libblas_:


#
# lapack
#
liblapack:  liblapack_$(LAPACK_LIBS_SWITCH)

liblapack_internal:
	 if test -d lapack ;  then ( cd lapack;  $(MAKE) ) ; fi

liblapack_external:
liblapack_:


#
# cleaning
#
clean distclean:
	 if test -d blas ;    then ( cd blas;    $(MAKE) $@ ) ; fi
	 if test -d lapack ;  then ( cd lapack;  $(MAKE) $@ ) ; fi

