--                                    --
-- �����ƥ�����ǡ����ɲ�             --
-- ���Ŵѻ�ˡ���б�                   --
--   ���������������               --
--   ���ౡ��Ͽ������������           --
--                                    --
-- Create Date : 2011/01/07           --
--                                    --

\set ON_ERROR_STOP

delete from tbl_syskanri where kanricd = '5101' and kbncd in ('10') and styukymd = '20100401' and  edyukymd = '99999999';
delete from tbl_syskanri where kanricd = '5112';

delete from tbl_syskanri where hospnum = 0;

COPY tbl_syskanri FROM stdin;
5101	10      	20100401	99999999	���Ŵѻ�����						0
5112	01      	20100401	99999999	         ����������						0
5112	02      	20100401	99999999	         ������������̤Ŭ�磱ǯ�����						0
5112	03      	20100401	99999999	         ������������̤Ŭ�磱ǯĶ��						0
5112	04      	20100401	99999999	         ����������						0
5112	05      	20100401	99999999	         �Ҳ�����������						0
5112	06      	20100401	99999999	         �Ҳ������������ʣ��������ʾ壱ǯ�����						0
5112	07      	20100401	99999999	         �Ҳ������������ʣ�ǯĶ��ǯ�������������						0
5112	08      	20100401	99999999	         �Ҳ������������ʣ�ǯ��������Ķ��						0
5112	09      	20100401	99999999	         �Ҳ�������������̤�вᣱǯĶ��						0
5112	10      	20100401	99999999	         �Ҳ�������������̤�вᣱǯ��������Ķ��						0
\.

insert into tbl_syskanri
select
a.kanricd,
a.kbncd,
a.styukymd,
a.edyukymd,
a.kanritbl,
a.termid,
a.opid,
a.creymd,
a.upymd,
a.uphms,
b.hospnum
from tbl_syskanri a,tbl_sysbase b
where
    a.hospnum = 0 ;

delete from tbl_syskanri where hospnum = 0;
