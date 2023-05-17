# Copyright (C) 2001-2016 Quantum ESPRESSO Foundation

AC_DEFUN([X_AC_QE_FFT_AUX], [

  # check size of pointers to int - needed to decide the size of integer
  # arrays in fortran holding C pointers for FFTW 

  #AC_LANG_POP(Fortran 77) 
  #AC_LANG_PUSH(C)
  #AC_PROG_CC($CC)

  #AC_CHECK_SIZEOF([int *])
  #SIZEOF_INT_P=$ac_cv_sizeof_int_p
  #AC_SUBST(SIZEOF_INT_P)

  #AC_LANG_POP(C)
  #AC_LANG_PUSH(Fortran 77) 

  ACX_POINTER_SIZE
  SIZEOF_INT_P=$ac_pointersize
  AC_SUBST(SIZEOF_INT_P)

 ]
)

