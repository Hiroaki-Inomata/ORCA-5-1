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
#     �����ɡ�1910����Ͽ
#     NOWYMD     ��������
#     NOWHMS     ���߻���
#     NOWDIR     ���ߥǥ��쥯�ȥ�
#     KAKUNASHI  ��ĥ�Ҥʤ��ե�����̾
#                ��ĥ�ҡ�exp or opt��
#------------------------------------------------------
NOWYMD=$(date +"%Y%m%d")
NOWHMS=$(date +"%H%M%S")
NOWDIR=$(pwd)

ln -s $SYSCONFDIR/dbgroup.inc dbgroup.inc

#���롼�׿����б���HOSPNUM�ο������¹ԡ�
SYSBASE=`${MONSQL} -dir directory -o CSV2 -c "SELECT hospnum FROM tbl_sysbase;"`
for HOSPNUM in $SYSBASE
do
  FILENAME=(`ls | grep exp$`)
  COUNT=(`ls | grep exp$ | wc -w`)
  SU=0
#�ե�����ο�������Ͽ�ץ�����¹Ԥ���
  while test ${SU} -lt ${COUNT}
  do
    KAKUNASHI=`echo ${FILENAME[${SU}]} | sed -e 's/.exp//'`
    echo "${PROGRAMID} .exp GO !! ${PREFNAME} ${HOSPNUM}" | logger
    $DBSTUB -dir directory -bd $PREFNAME $PROGRAMID -parameter ${HOSPNUM},${NOWYMD},${NOWHMS},${NOWDIR},${KAKUNASHI},"exp"

    SU=$(expr ${SU} + 1)
  done
done

#opt�ե������Ʊ�ͤ˹Ԥ�
#���롼�׿����б���HOSPNUM�ο������¹ԡ�
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
