--                                    --
-- �����ƥ�����ǡ�������             --
-- ����������ץץ����           --
-- �ѥ�᥿���󹹿�SQL                --
--                                    --
-- Create Date : 2008/07/11           --
--                                    --

\set ON_ERROR_STOP

delete from tbl_syskanri where kanricd = '0043' and kbncd in ('1012') and styukymd = '00000000' and  edyukymd = '99999999';

delete from tbl_syskanri where hospnum = 0;

COPY tbl_syskanri FROM stdin;
0043	1012    	00000000	99999999	�������ťǡ��������Ͽ                                                                                                                                          ORCBNYUALL              ����ǯ����          YMD       ������ʬ            PSN1      �ꥹ�ȶ�ʬ          PSN1                                                                                                                                                                                                                        111			20081203	20081203	153903	0
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
