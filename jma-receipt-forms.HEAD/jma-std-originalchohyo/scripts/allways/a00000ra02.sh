#!/bin/sh
. @jma-receipt-env@

PREFNAME=a00000ra00
PROGRAMID=A00000RA02
LOG_FILE="/var/log/jma-receipt/${12}a00000ra02"

PARAMFILE=/tmp/a00000ra02.txt
#-------------------------------------------#
#        $1    �����Уǡʲ���̾��
#        $2    ��������
#              1:�ɲ�  2:����  3:���
#        $3    �����ƥ�����
#        $4    ������
#        $5    ���ԣɣ�
#        $6    �����ֹ�
#        $7    ���Ų�
#        $8    �ɥ�����
#        $9    �ݸ��ȹ礻
#        ${10} ������Ģɼ̾
#        ${11} �ץ��̾
#        ${12} ���ŵ����ֹ�
#        ${13} ��ɼ�ֹ�
#-------------------------------------------#
#

	echo	"�����Уǡʲ���̾�ˡ�"$1 > $PARAMFILE
	echo	"����������"$2 >> $PARAMFILE
	echo    "�����ƥ����ա�"$3 >> $PARAMFILE
	echo    "��������"$4 >> $PARAMFILE
	echo    "���ԣɣġ�"$5 >> $PARAMFILE
	echo    "�����ֹ桧"$6 >> $PARAMFILE
	echo    "���Ųʡ�"$7 >> $PARAMFILE
	echo    "�ɥ�������"$8 >> $PARAMFILE
	echo    "�ݸ��ȹ礻��"$9 >> $PARAMFILE
	echo    "������Ģɼ̾��"${10} >> $PARAMFILE
	echo    "�ץ��̾��"${11} >> $PARAMFILE
	echo    "���ŵ����ֹ桧"${12} >> $PARAMFILE
	echo    "��ɼ�ֹ桧"${13} >> $PARAMFILE

#
	$DBSTUB -dir $LDDIRECTORY -bd $PREFNAME $PROGRAMID -parameter $1,$2,$3,$4,$5,$6,$7,$8,$9,${10},${11},${12},${13} > ${LOG_FILE}.log 2>&1
#
	exit
