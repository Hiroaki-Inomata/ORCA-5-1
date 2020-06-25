dnl Process this file with autoconf to produce a configure script.
AC_INIT
AC_CONFIG_SRCDIR([cobol/orca00/ORCGM01.CBL])

PACKAGE=jma-receipt
AC_SUBST(PACKAGE)
VERSION=`cat VERSION`
AC_SUBST(VERSION)

AC_PREFIX_DEFAULT(/usr/local/orca)

dnl --enable-debug
AC_ARG_ENABLE(debug, [  --enable-debug=[no/yes] turn on debugging [default=no]],,enable_debug="no")

if test "x$enable_debug" = "xyes"; then
  DEBUG="debug"
fi
AC_SUBST(DEBUG)

dnl Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_MAKE_SET


AC_PATH_PROG(LSB_RELEASE,lsb_release,no)
if test x$LSB_RELEASE = xno; then
  DISTCODENAME="unknown"
else
  DISTCODENAME="`lsb_release -sc`"
fi
AC_SUBST(DISTCODENAME)

AC_PATH_PROG(DPKG,dpkg,no)
if test x$DPKG = no; then
   ARCHITECTURE="i386"
else
   ARCHITECTURE="`dpkg --print-architecture`"
fi
AC_SUBST(ARCHITECTURE)

AC_PATH_PROG(PG_LSCLUSTERS,pg_lsclusters,no)
PGCLUSTER="main"
if test x$PG_LSCLUSTERS = xno; then
  PGVERSION="8.1"
  PGPATH="/var/lib/postgresql/8.1"
else
  PGVERSION=`pg_lsclusters | awk '{if ($2 == "main") {print $1}}'`
  PGPATH=`pg_lsclusters | awk '{if ($2 == "main") {print $6}}'|sed 's/\/main$//'`
fi
AC_SUBST(PGCLUSTER)
AC_SUBST(PGVERSION)
AC_SUBST(PGPATH)

PKG_PROG_PKG_CONFIG
PKG_CHECK_MODULES(png, "libpng12",[],
[
 	PKG_CHECK_MODULES(png, "libpng")
])
PKG_CHECK_MODULES(qrencode, "libqrencode")
PKG_CHECK_MODULES(xml2,
[
	libxml-2.0 >= 2.6.27
])
PKG_CHECK_MODULES(uuid,
[
	uuid >= 2.20.0
])

AC_SEARCH_LIBS([crypt],[crypt])

AC_PATH_PROG(PKG_CONFIG,pkg-config,no)
if test x$PKG_CONFIG = xno; then
	AC_MSG_ERROR(Couldn't find pkg-config)
fi

AC_ARG_WITH(patchdir,
	    [  --with-patchdir=DIR      patch libraries in DIR [PREFIX]],
            [patchdir=$withval],
            [patchdir='${prefix}'])

AC_ARG_WITH(patchlibdir,
	    [  --with-patchlibdir=DIR   patch cobol libraries in DIR [PREFIX/lib]],
            [patchlibdir=$withval],
            [patchlibdir='${prefix}/lib'])

AC_SUBST(patchdir)dnl
AC_SUBST(patchlibdir)dnl

AC_ARG_WITH(sitedir,
	    [  --with-sitedir=DIR      site libraries in DIR [PREFIX]],
            [sitedir=$withval],
            [sitedir='${prefix}'])

AC_ARG_WITH(sitesrcdir,
	    [  --with-sitesrcdir=DIR   site src libraries in DIR [PREFIX/site-src]],
            [sitesrcdir=$withval],
            [sitesrcdir='${prefix}/site-src'])

AC_ARG_WITH(sitelibdir,
	    [  --with-sitelibdir=DIR   site cobol libraries in DIR [PREFIX/lib]],
            [sitelibdir=$withval],
            [sitelibdir='${prefix}/lib'])

AC_SUBST(sitedir)dnl
AC_SUBST(sitesrcdir)dnl
AC_SUBST(sitelibdir)dnl

AC_MSG_CHECKING(OpenCOBOL compiler)
AC_ARG_WITH(open-cobol-prefix,
             [  --with-open-cobol-prefix=PFX   prefix where open-cobol is installed (optional)], 
  open_cobol_prefix="$withval", open_cobol_prefix="")

AC_ARG_WITH(open-cobol-exec-prefix,
             [  --with-open-cobol-exec-prefix=PFX Exec prefix where open-cobol is installed (optional)], 
  open_cobol_exec_prefix="$withval", open_cobol_exec_prefix="")

  if test x$open_cobol_exec_prefix != x ; then
    if test x${COBC+set} != xset; then
      COBC=$open_cobol_exec_prefix/bin/cobc08
      COB_CONFIG=$open_cobol_exec_prefix/bin/cob-config08
    fi
  fi
  if test x$open_cobol_prefix != x ; then
    if test x${COBC+set} != xset; then
      COBC=$open_cobol_prefix/bin/cobc08
      COB_CONFIG=$open_cobol_prefix/bin/cob-config08
    fi
  fi
  AC_PATH_PROG(COBC, cobc, no)
  AC_PATH_PROG(COB_CONFIG, cob-config, no)

  if test "$COBC" = "no" ; then
    no_cobc=yes
  else
    if test -x $COBC ; then
      no_cobc=""
    else
      no_cobc=yes
    fi
  fi
  if test "x$no_cobc" = x ; then
      AC_MSG_RESULT(yes)
      cobc_major_version=`$COB_CONFIG --version | \
	sed -n 's/\([[0-9]]*\)\.\([[0-9]]*\).*/\1/p'`
      cobc_minor_version=`$COB_CONFIG --version | \
	sed -n 's/\([[0-9]]*\)\.\([[0-9]]*\).*/\2/p'`
      if test "x${cobc_major_version}""${cobc_minor_version}" = x08; then
	COBOLFLAGS=" -m -dynamic -F "
	EXEC_COBOLFLAGS="-dynamic -F "
      elif test "x${cobc_major_version}" = x0  && \
        (expr "${cobc_minor_version}" \< "20") > /dev/null ; then
	  COBOLFLAGS="-m -dynamic -fixed "
  	  EXEC_COBOLFLAGS="-main -dynamic -fixed "
      else
	  COBOLFLAGS="-m -dynamic -fixed -fimplicit-init -std=cobol85 -Wall "
  	  EXEC_COBOLFLAGS="-x -dynamic -fixed -std=cobol85 -Wall "
	  if test "x$enable_debug" = "xyes"; then
	    COBOLFLAGS="${COBOLFLAGS} -debug -fdebugging-line -fsource-location "
	    EXEC_COBOLFLAGS="${EXEC_COBOLFLAGS} -debug -fdebugging-line "
	  fi
      fi
  else
      AC_MSG_RESULT(no)
      AC_MSG_ERROR(OpenCOBOL compiler not found.)
  fi

AC_SUBST(COBC)
AC_SUBST(COBOLFLAGS)
AC_SUBST(EXEC_COBOLFLAGS)

AC_MSG_CHECKING(MONTSUQI(panda))
AC_ARG_WITH(panda-dir,
             [  --with-panda-dir=[=DIR]  where panda is installed (optional)], 
  panda_dir="$withval", panda_dir="")

if test x$panda_dir != x; then
  dir=$panda_dir
else
  dir="/usr/local"
fi

dnl Checks for libraries.

dnl Checks for header files.
AC_HEADER_STDC

dnl Checks for typedefs, structures, and compiler characteristics.

dnl Checks for library functions.

dnl *************************************************************************** 
dnl for Ruby
dnl *************************************************************************** 
AC_ARG_WITH(ruby, 
[  --with-ruby=PATH        path to ruby],[
    AC_MSG_CHECKING(for ruby)
    RUBY="$withval"
    AC_MSG_RESULT($RUBY)
],[
    AC_MSG_CHECKING(for ruby)
    RUBY="/usr/bin/ruby"
    AC_MSG_RESULT(no)
])
AC_SUBST(RUBY) 

AC_CONFIG_FILES([
cobol/Makefile
cobol/Makefile.global
cobol/cutils/Makefile
cobol/copy/Makefile
bin/Makefile
lib/Makefile
lddef/Makefile
record/Makefile
screen/Makefile
form/Makefile
scripts/Makefile
data/Makefile
doc/Makefile
etc/Makefile
init/Makefile
tools/Makefile
Makefile
Makefile.directories
etc/jma-receipt.env
etc/jma-receipt.conf
etc/jppinfo.list
])
AC_OUTPUT
