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
if [ ${usrname} != "root" ] ; then
  echomsg "\nrootユーザで実行してください\n"
  exit 1
fi

echomsg "公開帳票 オリジナル帳票 アンインストール中..."

pgid=A00000L300

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


pgid=A00000KOHS

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


pgid=A21011A05

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


pgid=A01014M01

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


pgid=A21000H01A4

#/usr/local/site-jma-receipt/
for f in ${SITESRCDIR}/cobol/${pgid}* ; do
  rm -rf $f
done

#for f in ${SITESRCDIR}/cobol/copy/${pgid}* ; do
#  rm -rf $f
#done
rm -rf ${SITESRCDIR}/cobol/copy/A21000H01KA4.INC
rm -rf ${SITESRCDIR}/cobol/copy/A21000H01SA4.INC

#for f in ${SITESRCDIR}/form/${pgid}* ; do
#  rm -rf $f
#done
rm -rf ${SITESRCDIR}/form/A21000H01KA4.red
rm -rf ${SITESRCDIR}/form/A21000H01SA4.red

for f in ${SITESRCDIR}/doc/*${pgid}* ; do
  rm -rf $f
done

#/usr/lib/jma-receipt/site-lib/
for f in ${SITELIBDIR}/${pgid}* ; do
  rm -rf $f
done

#for f in ${SITEFORMDIR}/${pgid}* ; do
#  rm -rf $f
#done
rm -rf ${SITEFORMDIR}/A21000H01KA4.red
rm -rf ${SITEFORMDIR}/A21000H01SA4.red

for f in ${SITEDIR}/doc/*${pgid}* ; do
  rm -rf $f
done


pgid=A21000H01B5

#/usr/local/site-jma-receipt/
for f in ${SITESRCDIR}/cobol/${pgid}* ; do
  rm -rf $f
done

#for f in ${SITESRCDIR}/cobol/copy/${pgid}* ; do
#  rm -rf $f
#done
rm -rf ${SITESRCDIR}/cobol/copy/A21000H01KB5.INC
rm -rf ${SITESRCDIR}/cobol/copy/A21000H01SB5.INC

#for f in ${SITESRCDIR}/form/${pgid}* ; do
#  rm -rf $f
#done
rm -rf ${SITESRCDIR}/form/A21000H01KB5.red
rm -rf ${SITESRCDIR}/form/A21000H01SB5.red

for f in ${SITESRCDIR}/doc/*${pgid}* ; do
  rm -rf $f
done

#/usr/lib/jma-receipt/site-lib/
for f in ${SITELIBDIR}/${pgid}* ; do
  rm -rf $f
done

#for f in ${SITEFORMDIR}/${pgid}* ; do
#  rm -rf $f
#done
rm -rf ${SITEFORMDIR}/A21000H01KB5.red
rm -rf ${SITEFORMDIR}/A21000H01SB5.red

for f in ${SITEDIR}/doc/*${pgid}* ; do
  rm -rf $f
done


pgid=A00000H02

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


pgid=A21000H02

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


pgid=A00000H03

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


pgid=A00000H03A4

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


pgid=A00000H03A5

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


pgid=A00000H03NA4

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


pgid=A00000H03NA5

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


pgid=A21000H03

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


pgid=A00000H03A5P

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


pgid=A00000H03A5L

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


pgid=A00000YL01

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


pgid=A00000RA01

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


pgid=originalchohyo

for f in ${SITESRCDIR}/doc/*${pgid}* ; do
  rm -rf $f
done

for f in ${SITEDIR}/doc/*${pgid}* ; do
  rm -rf $f
done


echomsg "公開帳票 オリジナル帳票 アンインストール終了"

