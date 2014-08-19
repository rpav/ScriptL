dnl $Id$

dnl Bookkeeping
AC_PREREQ(2.52)

dnl Initialize autoconf and automake
AC_INIT([scriptlcom], [1.0.0])
AC_SUBST([PACKAGE_NAME])dnl

AM_INIT_AUTOMAKE
AC_CONFIG_HEADERS([defs.h])

dnl Various required programs
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

dnl Various required libraries

dnl Header checking
AC_HEADER_STDC
AC_CHECK_HEADERS([libintl.h limits.h stdlib.h])
AC_CHECK_HEADERS([sys/types.h])

dnl Typedef checking
AC_CHECK_TYPES([int8_t,  int16_t,  int32_t,  int64_t,
                u_int8_t, u_int16_t, u_int32_t, u_int64_t],,
                AC_MSG_ERROR([portable type not found.]))

dnl Library function checking
AC_FUNC_MALLOC
AC_CHECK_FUNCS([getopt_long])

AC_CHECK_LIB([readline], [readline])

dnl Miscellaneousness
AC_C_BIGENDIAN

dnl Flags
AC_ARG_ENABLE(debug, [  --enable-debug          enable debugging output [[default=no]]],
[
    if test $enableval = "yes";
     then test -z "$CFLAGS" && CFLAGS="-O0 -g3 -Wall -Werror"
     else CFLAGS="$CFLAGS -DNDEBUG"
    fi
], [
    CFLAGS="$CFLAGS -DNDEBUG"
])

dnl And output
AC_CONFIG_FILES([Makefile src/include/Makefile src/Makefile src/test/Makefile 
               ])

AC_OUTPUT
