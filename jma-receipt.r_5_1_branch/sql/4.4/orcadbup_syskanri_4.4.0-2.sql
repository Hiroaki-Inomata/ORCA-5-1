--                                    --
-- �����ƥ�����ǡ�������             --
-- ����������ץץ����           --
-- �ѥ�᥿���󹹿�SQL                --
--                                    --
-- Create Date : 2009/02/24           --
--                                    --

\set ON_ERROR_STOP

delete from tbl_syskanri where kanricd = '0044' and kbncd in ('1032') and styukymd = '00000000' and  edyukymd = '99999999';

delete from tbl_syskanri where hospnum = 0;

COPY tbl_syskanri FROM stdin;
0044	1032    	00000000	99999999	̤�����ɲ���̾����                                                                                                                                              ORCBG021                ����ǯ��            YM        ������ʬ            PSN1      ������ʬ            PSN1                                                                                                                                                                                                                        111			20090224	20090224	161056	0
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
