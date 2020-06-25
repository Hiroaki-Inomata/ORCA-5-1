#!/bin/sh

. /etc/jma-receipt/jma-receipt.env

prefno=38
prefname=ehime

usrname=`whoami`

# Etchで使用するコードUTF-8への変換
# echo message (default character-code EUC)
function echomsg() {
  if [ "${LANG}" = "ja_JP.UTF-8" ] || [ "${LANG}" = "ja_JP.UTF8" ] ||
      [ "${LANG}" = "ja_JP.utf8" ]; then
    if [ -z $2 ]; then
      echo `echo $1 | nkf -wE`
    else
      echo -n `echo $1 | nkf -wE`
    fi 
  else
    if [ -z $2 ]; then
      echo $1
    else
      echo -n $1
    fi
  fi
  return 0
}

# user check
if [ ${usrname} != "root" ] ; then
  echomsg "\nrootユーザで実行してください\n"
  exit 1
fi

echomsg "愛媛県(総括表・地方公費)アンインストール中..."

cp ${SYSCONFDIR}/kentan.inc ${SYSCONFDIR}/kentan.inc.bak
sed '/ehime/d' ${SYSCONFDIR}/kentan.inc.bak > ${SYSCONFDIR}/kentan.inc
rm -rf ${SYSCONFDIR}/kentan.inc.bak


for f in ${SITESRCDIR}/cobol/SEIKYU${prefno}* ; do
  rm -rf $f
done

for f in ${SITESRCDIR}/cobol/SOKATU${prefno}* ; do
  rm -rf $f
done

for f in ${SITESRCDIR}/cobol/copy/*SKT${prefno}* ; do
  rm -rf $f
done

for f in ${SITESRCDIR}/cobol/copy/SC${prefno}* ; do
  rm -rf $f
done

for f in ${SITESRCDIR}/cobol/copy/SEI${prefno}* ; do
  rm -rf $f
done

for f in ${SITESRCDIR}/form/SC${prefno}* ; do
  rm -rf $f
done

for f in ${SITESRCDIR}/form/SKT${prefno}* ; do
  rm -rf $f
done

if test -f ${SITESRCDIR}/lddef/${prefname}.bd ; then
  rm -rf ${SITESRCDIR}/lddef/${prefname}.bd 
fi

for f in ${SITESRCDIR}/record/SC${prefno}* ; do
  rm -rf $f
done

for f in ${SITESRCDIR}/record/SKT${prefno}* ; do
  rm -rf $f
done

for f in ${SITESRCDIR}/scripts/kentan/seikyu${prefno}* ; do
  rm -rf $f
done

for f in ${SITESRCDIR}/scripts/kentan/sokatu${prefno}* ; do
  rm -rf $f
done

for f in ${SITESRCDIR}/data/HKNJA${prefno}* ; do
  rm -rf $f
done

for f in ${SITESRCDIR}/data/HKNJAKOU${prefno}* ; do
  rm -rf $f
done

if test -f ${SITESRCDIR}/data/COMMENT.INI ; then
  rm -rf ${SITESRCDIR}/data/COMMENT.INI
fi

if test -f ${SITESRCDIR}/data/YUSEN.INI ; then
  rm -rf ${SITESRCDIR}/data/YUSEN.INI
fi

if test -f ${SITESRCDIR}/doc/orca_print_uninstall_${prefname}.sh ; then
  rm -rf ${SITESRCDIR}/doc/orca_print_uninstall_${prefname}.sh
fi


for f in ${SITELIBDIR}/SEIKYU${prefno}* ; do
  rm -rf $f
done

for f in ${SITELIBDIR}/SOKATU${prefno}* ; do
  rm -rf $f
done

for f in ${SITEFORMDIR}/SC${prefno}* ; do
  rm -rf $f
done

for f in ${SITEFORMDIR}/SKT${prefno}* ; do
  rm -rf $f
done

if test -f ${SITELDDEFDIR}/${prefname}.bd ; then
  rm -rf ${SITELDDEFDIR}/${prefname}.bd
fi

for f in ${SITERECORDDIR}/SC${prefno}* ; do
  rm -rf $f
done

for f in ${SITERECORDDIR}/SKT${prefno}* ; do
  rm -rf $f
done

for f in ${SITESCRIPTSDIR}/kentan/seikyu${prefno}* ; do
  rm -rf $f
done

for f in ${SITESCRIPTSDIR}/kentan/sokatu${prefno}* ; do
  rm -rf $f
done


if test -f ${LDDEFDIR}/${prefname}.bd ; then
  rm -rf ${LDDEFDIR}/${prefname}.bd
fi


for f in ${SITEDATADIR}/HKNJA${prefno}* ; do
  rm -rf $f
done

for f in ${SITEDATADIR}/HKNJAKOU${prefno}* ; do
  rm -rf $f
done

if test -f ${SITEDATADIR}/COMMENT.INI ; then
  rm -rf ${SITEDATADIR}/COMMENT.INI 
fi

if test -f ${SITEDATADIR}/YUSEN.INI ; then
  rm -rf ${SITEDATADIR}/YUSEN.INI 
fi

if test -f ${SITELIBDIR}/doc/orca_print_uninstall_${prefname}.sh ; then
  rm -rf ${SITELIBDIR}/doc/orca_print_uninstall_${prefname}.sh
fi


echomsg "愛媛県(総括表・地方公費)アンインストール終了！！"

