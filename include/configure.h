!
! Copyright (C) 2006 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!

!
! contains configure infos
!

#define  __HAVE_CONFIG_INFO

#define   __CONF_HOST           "@host@"
#define   __CONF_ARCH           "x86_64"

#define   __CONF_CC             "cc"
#define   __CONF_CFLAGS         "-O3"
#define   __CONF_DFLAGS         "-D__XSD -D__OPENMP -D__GFORTRAN -D__STD_F95 -D__FFTW"
#define   __CONF_CPP            "cpp"
#define   __CONF_CPPFLAGS       "-P -C -traditional"

#define   __CONF_F90            "gfortran"
#define   __CONF_MPIF90         "gfortran"
#define   __CONF_F90FLAGS       "$(FFLAGS) -x f95-cpp-input -fopenmp"
#define   __CONF_F77            "gfortran"
#define   __CONF_FFLAGS         "-O3 -g -fopenmp"
#define   __CONF_FFLAGS_NOOPT   "-O0 -g"
#define   __CONF_PRE_FDFLAGS    ""
#define   __CONF_FDFLAGS        "$(DFLAGS) $(MANUAL_DFLAGS)"

#define   __CONF_LD             "gfortran"
#define   __CONF_LDFLAGS        "-g -pthread -fopenmp"
#define   __CONF_IMOD           "-I"

#define   __CONF_BLAS_LIBS      "/home/centos/espresso-5.4.0-XML/BLAS/blas.a"
#define   __CONF_LAPACK_LIBS    "/home/centos/espresso-5.4.0-XML/lapack-3.2/lapack.a"
#define   __CONF_FFT_LIBS       ""
#define   __CONF_MASS_LIBS      ""

#define   __CONF_AR             "ar"
#define   __CONF_ARFLAGS        "ruv"
#define   __CONF_RANLIB         "ranlib"


