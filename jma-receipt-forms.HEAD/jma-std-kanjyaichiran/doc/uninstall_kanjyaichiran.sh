#!/bin/bash

. /etc/jma-receipt/jma-receipt.env

pgid=A00000L10

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

echomsg "公開帳票 患者一覧表 アンインストール中..."

#/usr/local/site-jma-receipt/
for f in ${SITESRCDIR}/cobol/${pgid}* ; do
  rm -rf $f
done

for f in ${SITESRCDIR}/cobol/copy/${pgid}* ; do
  rm -rf $f
done

for f in ${SITESRCDIR}/form/${pgid}* ; do
  rm -rf $f
done

for f in ${SITESRCDIR}/doc/*${pgid}* ; do
  rm -rf $f
done

#/usr/lib/jma-receipt/site-lib/
for f in ${SITELIBDIR}/${pgid}* ; do
  rm -rf $f
done

for f in ${SITEFORMDIR}/${pgid}* ; do
  rm -rf $f
done

for f in ${SITEDIR}/doc/*${pgid}* ; do
  rm -rf $f
done

pgid=kanjyaichiran

for f in ${SITESRCDIR}/doc/*${pgid}* ; do
  rm -rf $f
done

for f in ${SITEDIR}/doc/*${pgid}* ; do
  rm -rf $f
done

echomsg "公開帳票 患者一覧表 アンインストール終了"

