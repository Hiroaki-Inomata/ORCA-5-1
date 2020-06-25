#!/bin/bash
. @jma-receipt-env@

PREFNAME=okinawa
PROGRAMID=SOKATU4710
LOG_FILE="/var/log/jma-receipt/${15}sokatu4710"
RENNUM=0
#-------------------------------------------#
#    国保・診療報酬請求書（沖縄）
#        $1-${11}
#              印刷ＤＢ用定固定引数(CPORCSRTLNK.INC)
#        ${12} ジョブＩＤ
#        ${13} シェルＩＤ
#        ${14} 総括表の種類
#           0:全体  1:社保　2:国保
#        ${15} 医療機関番号
#        ${16} エラーファイル名 
#        ${17} 処理年月 
#        ${18} 対象保険者番号
#        ${19} 処理対象区分
#              0:全体  1:当月・月遅れ分  2:返戻分
#        ${20}-  ${21}医療機関コード別の処理指定時
#            該当診療年月の開始、終了
#-------------------------------------------#
#
##      エラーファイル削除
	echo $#
	echo "echo " ${MCP_TEMPDIR}/${16}
        if  [ -e ${MCP_TEMPDIR}/${16} ]; then
            rm  ${MCP_TEMPDIR}/${16}
        fi
        
##      返戻分
        if  [ ${19} -eq '0' ] || [ ${19} -eq '2' ]; then
            RENNUM=$(expr $RENNUM + 1) 
            $DBSTUB -dir $LDDIRECTORY -bd $PREFNAME $PROGRAMID -parameter $1,$2,$3,$RENNUM,$5,$6,$7,$8,$9,${10},${11},${15},${12},${13},${18},2,1,${20},${21},${16}
            if  [ -e ${MCP_TEMPDIR}/${16} ]; then
                exit
            fi
        fi
##      当月・月遅れ分
        if  [ ${19} -eq '0' ] || [ ${19} -eq '1' ]; then
            RENNUM=$(expr $RENNUM + 1) 
            $DBSTUB -dir $LDDIRECTORY -bd $PREFNAME $PROGRAMID -parameter $1,$2,$3,$RENNUM,$5,$6,$7,$8,$9,${10},${11},${15},${12},${13},${18},1,1,${20},${21},${16}
            if  [ -e ${MCP_TEMPDIR}/${16} ]; then
                exit
            fi
        fi
##      返戻分（特別療養費分）
        if  [ ${19} -eq '0' ] || [ ${19} -eq '2' ]; then
            RENNUM=$(expr $RENNUM + 1) 
            $DBSTUB -dir $LDDIRECTORY -bd $PREFNAME $PROGRAMID -parameter $1,$2,$3,$RENNUM,$5,$6,$7,$8,$9,${10},${11},${15},${12},${13},${18},2,2,${20},${21},${16}
            if  [ -e ${MCP_TEMPDIR}/${16} ]; then
                exit
            fi
        fi
##      当月・月遅れ分（特別療養費分）
        if  [ ${19} -eq '0' ] || [ ${19} -eq '1' ]; then
            RENNUM=$(expr $RENNUM + 1) 
            $DBSTUB -dir $LDDIRECTORY -bd $PREFNAME $PROGRAMID -parameter $1,$2,$3,$RENNUM,$5,$6,$7,$8,$9,${10},${11},${15},${12},${13},${18},1,2,${20},${21},${16}
            if  [ -e ${MCP_TEMPDIR}/${16} ]; then
                exit
            fi
        fi
        
	$DBSTUB  -dir $LDDIRECTORY -bd orcabt ORCBJOB -parameter JBE${12}${13},${15}

