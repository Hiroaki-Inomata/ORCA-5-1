#!/bin/bash
. @jma-receipt-env@

PREFNAME=nagano
PROGRAMID=SOKATU2010
LOG_FILE="/var/log/jma-receipt/${15}sokatu2010"
RENNUM=0
-------------------------------------------#
#    国保総括表作成（控え）（長野）
#        $1-${11}
#              印刷ＤＢ用定固定引数(CPORCSRTLNK.INC)
#        ${12} ジョブＩＤ
#        ${13} シェルＩＤ
#        ${15} 医療機関ＩＤ 
#        ${16} エラーファイル名 
#        ${19} 処理対象区分
#              0:全体  1:当月・月遅れ分  2:返戻分
#-------------------------------------------#
#
##      エラーファイル削除
	echo $#
	echo "echo " ${16}
        if  [ -e ${16} ]; then
            rm  ${16}
        fi
        
##      返戻分
        if  [ ${19} -eq '0' ] || [ ${19} -eq '2' ]; then
            RENNUM=$(expr $RENNUM + 1) 
            $DBSTUB -dir $LDDEFDIR/directory -bd $PREFNAME $PROGRAMID -parameter $1,$2,$3,$RENNUM,$5,$6,$7,$8,$9,${10},${11},${15},${12},${13},2,${16} > ${LOG_FILE}-2.log
            if  [ -e ${16} ]; then
                exit
            fi
        fi
##      当月・月遅れ分
        if  [ ${19} -eq '0' ] || [ ${19} -eq '1' ]; then
            RENNUM=$(expr $RENNUM + 1) 
            $DBSTUB -dir $LDDEFDIR/directory -bd $PREFNAME $PROGRAMID -parameter $1,$2,$3,$RENNUM,$5,$6,$7,$8,$9,${10},${11},${15},${12},${13},1,${16} > ${LOG_FILE}-1.log
            if  [ -e ${16} ]; then
                exit
            fi
        fi
        
	$DBSTUB  -dir $LDDEFDIR/directory -bd orcabt ORCBJOB -parameter JBE${12}${13},${15}

