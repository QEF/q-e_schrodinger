# Copyright (C) 2001-2016 Quantum ESPRESSO Foundation

AC_DEFUN([X_AC_QE_DTR], [

AC_ARG_ENABLE(dtr,
   [AS_HELP_STRING([--enable-dtr],
       [enable DTR output  (default: no)])],
   [if   test "$enableval" = "yes" ; then
      use_dtr=1
   else
      use_dtr=0
   fi],
   [use_dtr=0])
   
# preprocessing flag for XML
if test "$use_dtr" -eq 1 ; then
    try_dflags="$try_dflags -D__DTR"
    AC_PROG_CXX()
    ld_libs="$ld_libs -lstdc++ -ldl"
fi

])
