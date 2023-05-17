
AC_DEFUN([X_AC_PYTHON], [

  have_python=
  check_python=

  AC_ARG_WITH(python,
     [AS_HELP_STRING([--with-python],[Link with python library. (default: no)])],
     [try_python=$withval; check_python=yes ], [check_python=no])

 #
 pyldflags=""
 pyincludes=""
 #
 if test "$check_python" = "yes" ; then
   #
   #AC_MSG_CHECKING([python])
   #
   AC_CHECK_PROG(pyconfig, python3-config, python3-config)
   if test "x$pyconfig" = "x" ; then
     if test "x$try_python" != "x" ; then
        AC_CHECK_PROG(pyconfig, $try_python"-config", $try_python"-config")
     fi
   fi
   #
   if test "x$pyconfig" = "x" ; then
     have_python=no
   else
     echo setting PYCONFIG... $pyconfig
     pyldflags="`$pyconfig --ldflags`"
     pyincludes=`$pyconfig --includes`
     try_dflags="$try_dflags -D__PYTHON"
     have_python=yes
   fi
   #
   #AC_MSG_RESULT($have_python)
 fi
 
 AC_SUBST(pyldflags)
 AC_SUBST(pyincludes)

])

