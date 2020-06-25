#!/bin/bash

. /etc/jma-receipt/jma-receipt.env

usrname=`whoami`

# Etch�ǻ��Ѥ��륳����UTF-8�ؤ��Ѵ�
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
  echomsg "root�桼���Ǽ¹Ԥ��Ƥ�������"
  exit 1
fi

echomsg "���㸩�����ɽ����������˥ץ���ॳ�ԡ���..."

# file copy
for d in cobol doc data form lddef record scripts ; do
  cp -af ${d} ${SITESRCDIR}
done

# kentan.inc copy
cp ${SYSCONFDIR}/kentan.inc ${SYSCONFDIR}/kentan.inc.bak
sed '/niigata/d' ${SYSCONFDIR}/kentan.inc.bak > ${SYSCONFDIR}/kentan.inc
rm -rf ${SYSCONFDIR}/kentan.inc.bak
echo -e "\tniigata;" >> ${SYSCONFDIR}/kentan.inc

# site-upgrade.sh run
if [ -f ${SCRIPTSDIR}/allways/site-upgrade.sh ] ; then
	sh ${SCRIPTSDIR}/allways/site-upgrade.sh
else
	echomsg "���㸩�����ɽ����������˥ץ���ॳ�ԡ��۾ｪλ!!"
	exit
fi

sync
echomsg "���㸩�����ɽ����������˥ץ���ॳ�ԡ���λ!!"
