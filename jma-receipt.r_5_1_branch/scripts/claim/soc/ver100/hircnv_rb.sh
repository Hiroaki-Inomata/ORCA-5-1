#!/bin/bash

# XML�ǡ��������¹Ի��˻��Ѥ��볬�إǡ����ե����������(ver2��������)


INDIR=../soc/def
OUTDIR=../def
INHIRFILE=orca_decode_soc.hir
OUTHIRFILE=orca_decode.hir


# ���ǥ��쥯�ȥ�ΰ�ư
cd ../../rb2


echo 'claim�̿� �С������2�¹Ի��Ѥγ��إǡ����ե��������Ϥ��ޤ�'
echo '  ���ϥե�����̾ = ['$OUTHIRFILE']'
echo ''
# ����С��Ȥμ¹�
%RUBY% -Ke hircnv.rb $INDIR/$INHIRFILE $OUTDIR/$OUTHIRFILE > /dev/null

echo ''
echo 'done.'

