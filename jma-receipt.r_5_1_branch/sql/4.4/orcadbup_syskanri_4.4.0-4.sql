--                                    --
-- �����ƥ�����ǡ�������             --
-- ����������ץץ����           --
-- �ѥ�᥿���󹹿�SQL            --
--                                    --
-- Create Date : 2009/03/05           --
--                                    --

\set ON_ERROR_STOP

delete from tbl_syskanri where kanricd = '0044' and kbncd in ('1034') and styukymd = '00000000' and  edyukymd = '99999999';
delete from tbl_syskanri where kanricd = '0044' and kbncd in ('1035') and styukymd = '00000000' and  edyukymd = '99999999';
delete from tbl_syskanri where hospnum = 0;

COPY tbl_syskanri FROM stdin;
0044	1034    	00000000	99999999	���վ�����                                                                                                                                                    ORCBDR01                �����              YMD                                                                                                                                                                                                                                                                                     1			20090305	20090305	140000	0
0044	1035    	00000000	99999999	ͽ�������                                                                                                                                                    ORCBDR02                �����              YMD                                                                                                                                                                                                                                                                                     1			20090305	20090305	140000	0
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
