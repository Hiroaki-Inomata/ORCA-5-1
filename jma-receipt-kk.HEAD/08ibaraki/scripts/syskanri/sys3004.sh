#!/bin/bash
echo $0 | logger
echo `pwd`  | logger

JMARECEIPT_ENV="/etc/jma-receipt/jma-receipt.env"
if [ ! -f ${JMARECEIPT_ENV} ]; then
    echo "${JMARECEIPT_ENV} does not found."
    exit 1
fi
. $JMARECEIPT_ENV

HOSPNUM="01"

PREFNAME=sys3004
PROGRAMID=SYS3004

pushd ${MCP_TEMPDIR}/syskanri

# compile COBOL programs
MODULES=${PROGRAMID}.CBL
for f in $MODULES; do
  if test "x`echo -n $f | grep 'CBL$'`" != "x"; then
    m=`echo $f | sed 's/CBL$/so/'`
    echo -n "Building ${m}..."
    ${COBOL} ${COBOLFLAGS} -o ./${m} \
         -I ${PATCHCOPYDIR} \
         -I ${COPYDIR} \
         -I ${SITESRCDIR}/cobol/copy \
        ${f}
    if [ $? -ne 0 ]; then
      echo "${f} compile error" | logger
    fi
    echo "done"
  fi
done

#------------------------------------------------------
#     シス管「3004」登録
#     NOWYMD    現在日付
#     NOWHMS    現在時間
#     NOWDIR    現在ディレクトリ
#     FILENAME  ファイル名
#------------------------------------------------------
NOWYMD=$(date +"%Y%m%d")
NOWHMS=$(date +"%H%M%S")
NOWDIR=$(pwd)
FILENAME=sys3004.data

ln -s $SYSCONFDIR/dbgroup.inc dbgroup.inc

#グループ診療対応（HOSPNUMの数だけ実行）
SYSBASE=`${MONSQL} -dir directory -o CSV2 -c "SELECT hospnum FROM tbl_sysbase;"`
for HOSPNUM in $SYSBASE
do
# syskanri 3004 set
  echo "${PROGRAMID} GO !! ${PREFNAME} ${HOSPNUM}" | logger
  $DBSTUB -dir directory -bd $PREFNAME $PROGRAMID -parameter ${HOSPNUM},${NOWYMD},${NOWHMS},${NOWDIR},${FILENAME}
done

popd

exit 0
