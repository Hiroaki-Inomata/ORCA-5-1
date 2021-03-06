#!/bin/bash
. @jma-receipt-env@

PREFNAME=nagano
PROGRAMID=SEIKYU2003
LOG_FILE="/var/log/jma-receipt/${14}seikyu2003"
RENNUM=0
#-------------------------------------------#
#    国保総括表作成（長野）
#        $1-${11}
#              印刷ＤＢ用定固定引数(CPORCSRTLNK.INC)
#        ${12} ジョブＩＤ
#        ${13} シェルＩＤ
#        ${14} 医療機関番号
#        ${15} エラーファイル名 
#        ${16} 処理区分(０：一括、１：個別)
#        ${17} - ${18} 医療機関コード別の処理指定時
#                      該当診療年月の開始、終了
#-------------------------------------------#
#
##      エラーファイル削除
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
