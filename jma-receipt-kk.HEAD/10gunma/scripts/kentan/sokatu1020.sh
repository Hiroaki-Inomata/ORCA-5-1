#!/bin/bash
. @jma-receipt-env@

PREFNAME=gunma
PROGRAMID=SOKATU1020
LOG_FILE="/var/log/jma-receipt/${15}sokatu1020"
RENNUM=0
#-------------------------------------------#
#    ���ǥ����������ս�ʷ��ϡ�
#        $1-${11}
#              �����ģ�����������(CPORCSRTLNK.INC)
#        ${12} ����֣ɣ�
#        ${13} ������ɣ�
#        ${14} ���ɽ�μ���
#           0:����  1:���ݡ�2:����
#        ${15} ���ŵ����ֹ�
#        ${16} ���顼�ե�����̾ 
#        ${17} ����ǯ�� 
#        ${18} �о��ݸ����ֹ�
#        ${19} �����оݶ�ʬ
#              0:����  1:������٤�ʬ  2:����ʬ
#        ${20} - ${21} ���ŵ��إ������̤ν��������
#                      ��������ǯ��γ��ϡ���λ
#-------------------------------------------#
#
##      ���顼�ե�������
	echo $#
	echo "echo " ${MCP_TEMPDIR}/${16}
        if  [ -e ${MCP_TEMPDIR}/${16} ]; then
            rm  ${MCP_TEMPDIR}/${16}
        fi
        
##      ������٤�ʬ
        if  [ ${19} -eq '0' ] || [ ${19} -eq '1' ]; then
            RENNUM=$(expr $RENNUM + 1) 
            $DBSTUB -dir $LDDIRECTORY -bd $PREFNAME $PROGRAMID -parameter $1,$2,$3,$RENNUM,$5,$6,$7,$8,$9,${10},${11},${15},${12},${13},${18},1,1,${20},${21},${16}
            if  [ -e ${MCP_TEMPDIR}/${16} ]; then
                exit
            fi
        fi

	$DBSTUB  -dir $LDDIRECTORY -bd orcabt ORCBJOB -parameter JBE${12}${13},${15}

