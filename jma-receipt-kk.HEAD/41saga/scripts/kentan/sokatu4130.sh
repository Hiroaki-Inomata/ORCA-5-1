#!/bin/bash

. @jma-receipt-env@

PREFNAME=saga
PROGRAMID=SOKATU4130
LOG_FILE="/var/log/jma-receipt/${14}sokatu4130"
RENNUM=0
#-------------------------------------------#
#    ÀÁµá½ñºîÀ®¡Êº´²ì¡§¼ÒÊÝ ÆýÍÄ»ùÍÑ¡Ë
#        $1-${11}
#              °õºþ£Ä£ÂÍÑÄê¸ÇÄê°ú¿ô(CPORCSRTLNK.INC)
#        ${12} ¥¸¥ç¥Ö£É£Ä
#        ${13} ¥·¥§¥ë£É£Ä
#        ${14} °åÎÅµ¡´Ø£É£Ä 
#        ${15} ¥¨¥é¡¼¥Õ¥¡¥¤¥ëÌ¾ 
#        ${16} ½èÍý¶èÊ¬(£°¡§°ì³ç¡¢£±¡§¸ÄÊÌ)
#-------------------------------------------#
#
##      ¥¨¥é¡¼¥Õ¥¡¥¤¥ëºï½ü
	echo $#
	echo "echo " ${MCP_TEMPDIR}/${15}
        if  [ -e ${MCP_TEMPDIR}/${15} ]; then
            rm  ${MCP_TEMPDIR}/${15}
        fi

        cd  ${ORCA_DIR}

            RENNUM=$(expr ${RENNUM} + 1) 
            $DBSTUB -dir $LDDIRECTORY -bd $PREFNAME $PROGRAMID -parameter $1,$2,$3,$RENNUM,$5,$6,$7,$8,$9,${10},${11},${12},${13},${14},${16},${15} > ${LOG_FILE}.log 2>&1
            if  [ -e ${MCP_TEMPDIR}/${15} ]; then
                exit
            fi

        $DBSTUB  -dir $LDDIRECTORY -bd orcabt ORCBJOB -parameter JBE${12}${13},${14}

        exit
