--                                    --
-- ���׳Ф��񤭥ǡ�������             --
-- ����������ץץ����           --
--                                    --
-- Create Date : 2009/02/24           --
--                                    --

\set ON_ERROR_STOP

delete from tbl_toukeimemo where pgid = 'ORCBG021' and kanricd = '0000' and kbncd = '000';
delete from tbl_toukeimemo where hospnum = 0;

COPY tbl_toukeimemo FROM stdin;
ORCBG021	0000	000     	00000000	99999999									����ǯ�������ǯ������Ϥ��ޤ���\n������ʬ�����������Ƥ���̾���оݤȤ��ޤ���\n�������������������������ɤޤ�����̾�����ɤ����Ϥ��줿\n������������������̾���оݤȤ��ޤ���\n������ʬ�����������Ϥ��줿����ǯ��˿��Ť����봵�ԤΤ�\n�����������������оݤȤ��ޤ���\n�������������������Ƥδ��Ԥ��оݤȤ��ޤ���\n\n�����Ϥ��줿����ǯ��������ž���Ȥʤä���̾�Ͻ����оݳ��Ǥ���\n\n\n			20090224	20090224	150000	0	 		
\.

insert into tbl_toukeimemo select a.pgid,a.kanricd,a.kbncd,a.styukymd,a.edyukymd,a.para1,a.para2,a.para3,a.para4,a.para5,
a.para6,a.para7,a.para8,a.memo,a.termid,a.opid,to_char(now(),'yyyymmdd'),to_char(now(),'yyyymmdd'),to_char(now(),'hh24miss'),
b.hospnum,a.savepara,a.para9,a.para10
from tbl_toukeimemo a,tbl_sysbase b where a.hospnum = 0;

delete from tbl_toukeimemo where hospnum = 0;
