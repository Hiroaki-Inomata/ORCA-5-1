#!/bin/bash
. @jma-receipt-env@

PREFNAME=kanagawa
PROGRAMID=SOKATU1400
LOG_FILE="/var/log/jma-receipt/${15}sokatu1400"
RENNUM=0
#-------------------------------------------#
#    ���ݡ�����������ɽ�ʿ������
#        $1-${11}
#              �����ģ�����������(CPORCSRTLNK.INC)
#        ${12} ����֣ɣ�
#        ${13} ������ɣ�
#        ${15} ���ŵ����ֹ�
#        ${16} ���顼�ե�����̾ 
#        ${19} �����оݶ�ʬ
#              0:����  1:������٤�ʬ  2:����ʬ
#        ${20}-  ${21}���ŵ��إ������̤ν��������
#            ��������ǯ��γ��ϡ���λ
#-------------------------------------------#
#
##      ���顼�ե�������
	echo $#
	echo "echo " ${MCP_TEMPDIR}/${16}
        if  [ -e ${MCP_TEMPDIR}/${16} ]; then
            rm  ${MCP_TEMPDIR}/${16}
        fi
        
##      ����ʬ
        if  [ ${19} -eq '0' ] || [ ${19} -eq '2' ]; then
            RENNUM=$(expr $RENNUM + 1) 
            $DBSTUB -dir $LDDIRECTORY -bd $PREFNAME $PROGRAMID -parameter $1,$2,$3,$RENNUM,$5,$6,$7,$8,$9,${10},${11},${15},${12},${13},2,1,${20},${21},${16}
            if  [ -e ${MCP_TEMPDIR}/${16} ]; then
                exit
            fi
        fi
##      ������٤�ʬ
        if  [ ${19} -eq '0' ] || [ ${19} -eq '1' ]; then
            RENNUM=$(expr $RENNUM + 1) 
            $DBSTUB -dir $LDDIRECTORY -bd $PREFNAME $PROGRAMID -parameter $1,$2,$3,$RENNUM,$5,$6,$7,$8,$9,${10},${11},${15},${12},${13},1,1,${20},${21},${16}
            if  [ -e ${MCP_TEMPDIR}/${16} ]; then
                exit
            fi
        fi
##      ����ʬ������������ʬ��
        if  [ ${19} -eq '0' ] || [ ${19} -eq '2' ]; then
            RENNUM=$(expr $RENNUM + 1) 
            $DBSTUB -dir $LDDIRECTORY -bd $PREFNAME $PROGRAMID -parameter $1,$2,$3,$RENNUM,$5,$6,$7,$8,$9,${10},${11},${15},${12},${13},2,2,${20},${21},${16}
            if  [ -e ${MCP_TEMPDIR}/${16} ]; then
                exit
            fi
        fi
##      ������٤�ʬ������������ʬ��
        if  [ ${19} -eq '0' ] || [ ${19} -eq '1' ]; then
            RENNUM=$(expr $RENNUM + 1) 
            $DBSTUB -dir $LDDIRECTORY -bd $PREFNAME $PROGRAMID -parameter $1,$2,$3,$RENNUM,$5,$6,$7,$8,$9,${10},${11},${15},${12},${13},1,2,${20},${21},${16}
            if  [ -e ${MCP_TEMPDIR}/${16} ]; then
                exit
            fi
        fi
        
	$DBSTUB  -dir $LDDIRECTORY -bd orcabt ORCBJOB -parameter JBE${12}${13},${15}
