--                                    --
-- ���׳Ф��񤭥ǡ�������             --
-- ����������ץץ����           --
--                                    --
-- Create Date : 2009/02/25           --
--                                    --

\set ON_ERROR_STOP

delete from tbl_toukeimemo where pgid = 'ORCBMSTCOPY' and kanricd = '0000' and kbncd = '000';
delete from tbl_toukeimemo where hospnum = 0;

COPY tbl_toukeimemo FROM stdin;
ORCBMSTCOPY	0000	000     	00000000	99999999									������ʬ�����������ϥ����ɤ�ʣ��\n���������������������ޥ�����ʣ��\n�������������������ϥ��åȤ�ʣ��\n\nʣ�̸����ŵ��ء���ʣ�̸��Ȥ���¾���ΰ��ŵ��ؼ����ֹ�ʿ��������\n����������������������ꤷ�ޤ���\n\n\n			20090225	20090225	100000	0	 		
\.

insert into tbl_toukeimemo select a.pgid,a.kanricd,a.kbncd,a.styukymd,a.edyukymd,a.para1,a.para2,a.para3,a.para4,a.para5,
a.para6,a.para7,a.para8,a.memo,a.termid,a.opid,to_char(now(),'yyyymmdd'),to_char(now(),'yyyymmdd'),to_char(now(),'hh24miss'),
b.hospnum,a.savepara,a.para9,a.para10
from tbl_toukeimemo a,tbl_sysbase b where a.hospnum = 0;

delete from tbl_toukeimemo where hospnum = 0;
