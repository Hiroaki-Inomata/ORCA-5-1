--                                    --
-- ���׳Ф��񤭥ǡ�������             --
-- ����������ץץ����           --
--                                    --
-- Create Date : 2009/03/05           --
--                                    --

\set ON_ERROR_STOP

delete from tbl_toukeimemo where pgid = 'ORCBDR01' and kanricd = '0000' and kbncd = '000';
delete from tbl_toukeimemo where pgid = 'ORCBDR02' and kanricd = '0000' and kbncd = '000';
delete from tbl_toukeimemo where hospnum = 0;

COPY tbl_toukeimemo FROM stdin;
ORCBDR01	0000	000     	00000000	99999999									����������ꤵ�줿���հ����μ��վ���������ޤ���\n\n������ϸ��ߤ����դ�ꣲ����ʾ��������դ���ꤷ�Ʋ�������\n			20090305	20090305	140000	0	 		
ORCBDR02	0000	000     	00000000	99999999									����������ꤵ�줿���հ�����ͽ�����������ޤ���\n\n������ϸ��ߤ����դ�ꣲ����ʾ��������դ���ꤷ�Ʋ�������\n			20090305	20090305	140000	0	 		
\.

insert into tbl_toukeimemo select a.pgid,a.kanricd,a.kbncd,a.styukymd,a.edyukymd,a.para1,a.para2,a.para3,a.para4,a.para5,
a.para6,a.para7,a.para8,a.memo,a.termid,a.opid,to_char(now(),'yyyymmdd'),to_char(now(),'yyyymmdd'),to_char(now(),'hh24miss'),
b.hospnum,a.savepara,a.para9,a.para10
from tbl_toukeimemo a,tbl_sysbase b where a.hospnum = 0;

delete from tbl_toukeimemo where hospnum = 0;
