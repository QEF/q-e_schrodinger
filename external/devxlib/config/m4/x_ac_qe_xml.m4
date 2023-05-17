# Copyright (C) 2001-2016 Quantum ESPRESSO Foundation

AC_DEFUN([X_AC_QE_XML], [

AC_ARG_ENABLE(xml,
   [AS_HELP_STRING([--enable-xml],
       [enable XML output  (default: yes)])],
   [if   test "$enableval" = "yes" ; then
      use_xml=1
   else
      use_xml=0
   fi],
   [use_xml=1])
   
# preprocessing flag for XML
if test "$use_xml" -eq 0 ; then try_dflags="$try_dflags -D__OLDXML" ; fi

])
