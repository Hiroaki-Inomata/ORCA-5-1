#!/bin/bash

# XMLデータ受信実行時に使用する階層データファイルの生成(ver2形式出力)


INDIR=../soc/def
OUTDIR=../def
INHIRFILE=orca_decode_soc.hir
OUTHIRFILE=orca_decode.hir


# 基準ディレクトリの移動
cd ../../rb2


echo 'claim通信 バージョン2実行時用の階層データファイルを出力します'
echo '  出力ファイル名 = ['$OUTHIRFILE']'
echo ''
# コンバートの実行
%RUBY% -Ke hircnv.rb $INDIR/$INHIRFILE $OUTDIR/$OUTHIRFILE > /dev/null

echo ''
echo 'done.'

