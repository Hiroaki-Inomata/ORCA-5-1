#!/bin/bash

. @jma-receipt-env@

PREFNAME=kanagawa
PROGRAMID=SEIKYU1403
LOG_FILE="/var/log/jma-receipt/${14}seikyu1403"
RENNUM=0
#-------------------------------------------#
#    ������������ʿ����������
#        $1-${11}
#              �����ģ�����������(CPORCSRTLNK.INC)
#        ${12} ����֣ɣ�
#        ${13} ������ɣ�
#        ${14} ���ŵ����ֹ�
#        ${15} ���顼�ե�����̾ 
#        ${16} ������ʬ(������硢��������)
#        ${17}-  ${18}���ŵ��إ������̤ν��������
#            ��������ǯ��γ��ϡ���λ
#-------------------------------------------#
#
##      ���顼�ե�������
	echo $#
	echo "echo " ${MCP_TEMPDIR}/${15}
        if  [ -e ${MCP_TEMPDIR}/${15} ]; then
            rm  ${MCP_TEMPDIR}/${15}
        fi

        cd  ${ORCA_DIR}

            RENNUM=$(expr ${RENNUM} + 1) 
            $DBSTUB -dir $LDDIRECTORY -bd $PREFNAME $PROGRAMID -parameter $1,$2,$3,$RENNUM,$5,$6,$7,$8,$9,${10},${11},${12},${13},${14},${16},${17},${18},${15}
            if  [ -e ${MCP_TEMPDIR}/${15} ]; then
                exit
            fi

        $DBSTUB  -dir $LDDIRECTORY -bd orcabt ORCBJOB -parameter JBE${12}${13},${14}

        exit
