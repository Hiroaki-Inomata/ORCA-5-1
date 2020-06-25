#!/bin/bash

. /etc/jma-receipt/jma-receipt.env

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
if [ $usrname != "root" ] ; then
  echomsg "rootユーザで実行してください"
  exit 1
fi

echomsg "埼玉県（総括表・地方公費）プログラムコピー中..."

# file copy
for d in cobol doc data form lddef record scripts ; do
  cp -af ${d} ${SITESRCDIR}
done

# kentan.inc copy
cp ${SYSCONFDIR}/kentan.inc ${SYSCONFDIR}/kentan.inc.bak
sed '/saitama/d' ${SYSCONFDIR}/kentan.inc.bak > ${SYSCONFDIR}/kentan.inc
rm -rf ${SYSCONFDIR}/kentan.inc.bak
echo -e "\tsaitama;" >> ${SYSCONFDIR}/kentan.inc

# site-upgrade.sh run
if [ -f ${SCRIPTSDIR}/allways/site-upgrade.sh ] ; then
	sh ${SCRIPTSDIR}/allways/site-upgrade.sh
else
	echomsg "埼玉県（総括表・地方公費）プログラムコピー異常終了!!"
	exit
fi

#不要ＰＧ（ORCBM022,ORCBM037,ORCBM038）の削除
for f in ${SITESRCDIR}/cobol/ORCBM022.CBL ; do
  rm -rf $f
done
for f in ${SITESRCDIR}/cobol/ORCBM037.CBL ; do
  rm -rf $f
done
for f in ${SITESRCDIR}/cobol/ORCBM038.CBL ; do
  rm -rf $f
done

for f in ${SITELIBDIR}/ORCBM022.so ; do
  rm -rf $f
done
for f in ${SITELIBDIR}/ORCBM037.so ; do
  rm -rf $f
done
for f in ${SITELIBDIR}/ORCBM038.so ; do
  rm -rf $f
done

sync
echomsg "埼玉県（総括表・地方公費）プログラムコピー終了!!"
