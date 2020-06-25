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

# check user
if [ $usrname != "root" ] ; then
  echomsg "root�桼���Ǽ¹Ԥ��Ƥ�������"
  exit 1
fi

# start message
echomsg "����Ģɼ ���԰���ɽ �ץ���ॳ�ԡ���..."

# file copy
for d in cobol form doc ; do
  cp -af ${d} ${SITESRCDIR}
done

# run site-upgrade.sh
if [ -f ${SCRIPTSDIR}/allways/site-upgrade.sh ] ; then
  bash ${SCRIPTSDIR}/allways/site-upgrade.sh
else
  echomsg "����Ģɼ ���԰���ɽ �ץ���ॳ�ԡ��۾ｪλ!!"
  exit 1
fi

# end message
sync
echomsg "����Ģɼ ���԰���ɽ �ץ���ॳ�ԡ���λ!!"
exit 0
