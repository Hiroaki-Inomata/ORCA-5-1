--                                    --
-- �����ƥ�����ǡ�������             --
-- ����ץץ���� 0044            --
--                                    --
--                                    --
-- Create Date : 2008/06/02           --
--                                    --

\set ON_ERROR_STOP

delete from tbl_syskanri where kanricd = '0044' and kbncd = '1029';
delete from tbl_syskanri where kanricd = '0044' and kbncd = '1007';
delete from tbl_syskanri where kanricd = '0044' and kbncd = '1001';
delete from tbl_syskanri where hospnum = 0;

COPY tbl_syskanri (kanricd, kbncd, styukymd, edyukymd, kanritbl, termid, opid, creymd, upymd, uphms, hospnum) FROM stdin;
0044	1029     	00000000	99999999	���٤�ʬ�쥻�ťǡ���̤�������԰���                                                                                                                              ORCBG019                ����ǯ��            YM                                                                                                                                                                                                                                                                                      1			20080530	20080530	155116	0
0044	1007    	00000000	99999999	����ƣ����ѻ������                                                                                                                                          ORCBZ002                ���ų���ǯ��        YM        ���Ž�λǯ��        YM        ���Ųʥ�����        PSN2      ������ʬ            PSN1      �����ֹ�            PTNUM                                                                                                                                                           11212			20041006	20080806	133718	0
0044	1001    	00000000	99999999	��ץ�����                                                                                                                                                      ORCBG007                ����ǯ��            YM        �����ֹ�            PTNUM     ������ʬ            PSN1      �����ֹ�            BTUNUM    �¤ӽ�              PSN1      ���Ųʶ�ʬ          PSN1      ���Ų�              SRYKA     �ݸ���ʬ            PSN1      ������              DAY       ��λ��              DAY       1212222222			20041006	20080808	143331	0
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


