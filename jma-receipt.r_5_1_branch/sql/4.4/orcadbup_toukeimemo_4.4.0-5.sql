--                                    --
-- ���׳Ф��񤭥ǡ�������             --
-- ����������ץץ����           --
--                                    --
-- Create Date : 2009/09/11           --
--                                    --

\set ON_ERROR_STOP

delete from tbl_toukeimemo where pgid = 'ORCBGCHILDBIRTH' and kanricd = '0000' and kbncd = '000';
delete from tbl_toukeimemo where pgid = 'ORCBGCHILDLST' and kanricd = '0000' and kbncd = '000';
delete from tbl_toukeimemo where hospnum = 0;

COPY tbl_toukeimemo FROM stdin;
ORCBGCHILDBIRTH	0000	000     	00000000	99999999									����ǯ����ꤵ�줿����ǯ��δ��Ԥ�������ޤ���\n\n����衡����������ꤷ�Ʋ��������������ݵڤӹ���\n������������������������������������������\n������������������������������������������\n\n\n			20090911	20090911	140000	0	 		
ORCBGCHILDLST	0000	000     	00000000	99999999									����ǯ����ꤵ�줿����ǯ��δ��Ԥ�������ޤ���������ǯ��λ��̵꤬�����Ͻл�����������̤�\n������������������������������������������������������ǯ�̤����δ��Ԥ�������ޤ�����\n\n������ʬ���оݴ��Ԥ���ꤷ�Ʋ�����������������\n��������������������������������������������Ѥ�\n����������������������������������������̤����\n�������������������������������������������ᤷ�ʤ�\n\n��������ʬ������ǯ����ꤵ�줿���Τ�ͭ���Ǥ�\n\n\n			20090911	20090911	140000	0	 		
\.

insert into tbl_toukeimemo select a.pgid,a.kanricd,a.kbncd,a.styukymd,a.edyukymd,a.para1,a.para2,a.para3,a.para4,a.para5,
a.para6,a.para7,a.para8,a.memo,a.termid,a.opid,to_char(now(),'yyyymmdd'),to_char(now(),'yyyymmdd'),to_char(now(),'hh24miss'),
b.hospnum,a.savepara,a.para9,a.para10
from tbl_toukeimemo a,tbl_sysbase b where a.hospnum = 0;

delete from tbl_toukeimemo where hospnum = 0;
