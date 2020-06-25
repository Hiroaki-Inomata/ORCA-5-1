#!/bin/bash

# ����XML�ǡ��������ǻ��Ѥ���def/template�ե����������(ver1��������)


INDEFDIR=../../def
INTEMPLATEDIR=../../template
OUTDEFDIR=../../../def
OUTTEMPLATEDIR=../../../template
INDEFFILE=orca_layout_front_soc.def
INTEMPLATEFILE=orca_base_front_soc.xml
OUTDEFFILE=orca_layout_front.def
OUTTEMPLATEFILE=orca_base_front.xml


# ���ǥ��쥯�ȥ�ΰ�ư
cd rb


echo 'claim�̿� �С������1�¹Ի��ѥ���С���'
echo 'ver2��������def�������ե����뤫�顢ver1����def�ե�������������ޤ�'
echo '  ����def�ե�����̾ = ['$OUTDEFFILE']'
echo ''
STARTTIME=`date '+%T'`
# ����С��Ȥμ¹�
%RUBY% -Ke claim_convert.rb -D $INDEFDIR/$INDEFFILE $OUTDEFDIR/$OUTDEFFILE

echo ''
echo 'ver2����XML�ƥ�ץ졼�ȥ������ե����뤫�顢ver1����XML�ƥ�ץ졼�ȥե�������������ޤ�'
echo '  ���ϥƥ�ץ졼�ȥե�����̾ = ['$OUTTEMPLATEFILE']'
echo ''
XMLSTARTTIME=`date '+%T'`
%RUBY% -Ke claim_convert.rb -X $INTEMPLATEDIR/$INTEMPLATEFILE $OUTTEMPLATEDIR/$OUTTEMPLATEFILE


echo ''
echo 'done.'
echo ''
ENDTIME=`date '+%T'`
echo 'StartTime =[' $STARTTIME ']'
echo 'EndTime =[' $ENDTIME ']'

