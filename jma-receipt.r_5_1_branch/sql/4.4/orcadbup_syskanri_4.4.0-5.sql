--                                    --
-- �����ƥ�����ǡ�������             --
-- ����������ץץ����           --
-- �ѥ�᥿���󹹿�SQL            --
--                                    --
-- Create Date : 2009/09/11           --
--                                    --

\set ON_ERROR_STOP

delete from tbl_syskanri where kanricd = '0044' and kbncd in ('1036') and styukymd = '00000000' and  edyukymd = '99999999';
delete from tbl_syskanri where kanricd = '0044' and kbncd in ('1037') and styukymd = '00000000' and  edyukymd = '99999999';

delete from tbl_syskanri where hospnum = 0;

COPY tbl_syskanri FROM stdin;
0044	1036    	00000000	99999999	�л��������������                                                                                                                                            ORCBGCHILDBIRTH         ����ǯ��            YM        �����              PSN1                                                                                                                                                                                                                                                      11			20090911	20090911	100000	0
0044	1037    	00000000	99999999	�л�������������å��ꥹ��                                                                                                                                    ORCBGCHILDLST           ����ǯ��            YM        ������ʬ            PSN1                                                                                                                                                                                                                                                      22			20090911	20090911	100000	0
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
