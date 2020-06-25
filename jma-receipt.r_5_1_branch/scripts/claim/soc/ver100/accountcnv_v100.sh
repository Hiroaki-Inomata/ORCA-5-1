#!/bin/bash

# 会計XMLデータ送信で使用するdef/templateファイルの生成(ver1形式出力)


INDEFDIR=../../def
INTEMPLATEDIR=../../template
OUTDEFDIR=../../../def
OUTTEMPLATEDIR=../../../template
INDEFFILE=orca_layout_accounting_soc.def
INTEMPLATEFILE=orca_base_accounting_soc.xml
OUTDEFFILE=orca_layout_accounting.def
OUTTEMPLATEFILE=orca_base_accounting.xml


# 基準ディレクトリの移動
cd rb


echo 'claim通信 バージョン1実行時用コンバート'
echo 'ver2形式会計defソースファイルから、ver1形式defファイルを生成します'
echo '  出力defファイル名 = ['$OUTDEFFILE']'
echo ''
STARTTIME=`date '+%T'`
# コンバートの実行
%RUBY% -Ke claim_convert.rb -D $INDEFDIR/$INDEFFILE $OUTDEFDIR/$OUTDEFFILE

echo ''
echo 'ver2形式XMLテンプレートソースファイルから、ver1形式XMLテンプレートファイルを生成します'
echo '  出力テンプレートファイル名 = ['$OUTTEMPLATEFILE']'
echo ''
XMLSTARTTIME=`date '+%T'`
%RUBY% -Ke claim_convert.rb -X $INTEMPLATEDIR/$INTEMPLATEFILE $OUTTEMPLATEDIR/$OUTTEMPLATEFILE


echo ''
echo 'done.'
echo ''
ENDTIME=`date '+%T'`
echo 'StartTime =[' $STARTTIME ']'
echo 'EndTime =[' $ENDTIME ']'

