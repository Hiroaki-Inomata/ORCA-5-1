#!/bin/bash

. /etc/jma-receipt/jma-receipt.env

PARAMFILE=/tmp/userpgparam.txt
#-------------------------------------------#
#    �桼���ץ���൯ư�ʥ���ץ륹����ץȡ�
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
#        ${12} ���ŵ��ؼ����ֹ�
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
	echo    "���ŵ��ؼ����ֹ桧"${12} >> $PARAMFILE
	echo    "��ɼ�ֹ桧"${13} >> $PARAMFILE

#
#       $DBSTUB -dir $LDDEFDIR/directory -bd xxxxxxxx XXXXXXXX -parameter $1,$2,$3,$4,$5,$6,$7,$8,$9,${10},${11},${12},${13}
#        
	exit
