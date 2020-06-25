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

PREFNAME=prgoption
PROGRAMID=PRGOPTIONSET

pushd ${MCP_TEMPDIR}/${PREFNAME}

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
#     シス管「1910」登録
#     NOWYMD     現在日付
#     NOWHMS     現在時間
#     NOWDIR     現在ディレクトリ
#     KAKUNASHI  拡張子なしファイル名
#                拡張子（exp or opt）
#------------------------------------------------------
NOWYMD=$(date +"%Y%m%d")
NOWHMS=$(date +"%H%M%S")
NOWDIR=$(pwd)

ln -s $SYSCONFDIR/dbgroup.inc dbgroup.inc

#グループ診療対応（HOSPNUMの数だけ実行）
SYSBASE=`${MONSQL} -dir directory -o CSV2 -c "SELECT hospnum FROM tbl_sysbase;"`
for HOSPNUM in $SYSBASE
do
  FILENAME=(`ls | grep exp$`)
  COUNT=(`ls | grep exp$ | wc -w`)
  SU=0
#ファイルの数だけ登録プログラムを実行する
  while test ${SU} -lt ${COUNT}
  do
    KAKUNASHI=`echo ${FILENAME[${SU}]} | sed -e 's/.exp//'`
    echo "${PROGRAMID} .exp GO !! ${PREFNAME} ${HOSPNUM}" | logger
    $DBSTUB -dir directory -bd $PREFNAME $PROGRAMID -parameter ${HOSPNUM},${NOWYMD},${NOWHMS},${NOWDIR},${KAKUNASHI},"exp"

    SU=$(expr ${SU} + 1)
  done
done

#optファイルも同様に行う
#グループ診療対応（HOSPNUMの数だけ実行）
for HOSPNUM in $SYSBASE
do
  FILENAME=(`ls | grep opt$`)
  COUNT=(`ls | grep opt$ | wc -w`)
  SU=0

  while test ${SU} -lt ${COUNT}
  do
    KAKUNASHI=`echo ${FILENAME[${SU}]} | sed -e 's/.opt//'`
    echo "${PROGRAMID} .opt GO !! ${PREFNAME} ${HOSPNUM}" | logger
    $DBSTUB -dir directory -bd $PREFNAME $PROGRAMID -parameter ${HOSPNUM},${NOWYMD},${NOWHMS},${NOWDIR},${KAKUNASHI},"opt"

    SU=$(expr ${SU} + 1)
  done
done

popd

exit 0
