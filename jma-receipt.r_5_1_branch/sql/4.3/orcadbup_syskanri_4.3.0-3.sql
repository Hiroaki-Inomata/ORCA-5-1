--                                    --
-- �����ƥ�����ǡ�������             --
-- ����������ץץ����           --
-- �ѥ�᥿���󹹿�SQL                --
--                                    --
-- Create Date : 2008/07/11           --
--                                    --

\set ON_ERROR_STOP

delete from tbl_syskanri where kanricd = '0043' and kbncd in ('1010') and styukymd = '00000000' and  edyukymd = '99999999';
delete from tbl_syskanri where kanricd = '0044' and kbncd in ('1023','1024','1030','1031') and styukymd = '00000000' and  edyukymd = '99999999';

delete from tbl_syskanri where hospnum = 0;

COPY tbl_syskanri FROM stdin;
0043	1010    	00000000	99999999	��Ǽ����                                                                                                                                                        ORCBD010                ������              YMD       ��λ������          YMD       ������ʬ            PSN1      ���׶�ʬ            PSN1      ���ٶ�ʬ            PSN1      ���ɽ������ʬ      PSN1                                                                                                                              121111    1			20070209	20080710	160049	0
0044	1023    	00000000	99999999	������Ź԰ٷ��Ĵ                                                                                                                                              ORCBG016                ����ǯ��            YM        ������ʬ            PSN1      ���Ų�              SRYKA     ���Ź԰٣�          SRYCD     ���Ź԰٣�          SRYCD     ���Ź԰٣�          SRYCD     ���Ź԰٣�          SRYCD     ���Ź԰٣�          SRYCD     ���Ź԰٣�          SRYCD     ���Ź԰٣�          SRYCD     1121222222			20060622	20080905	143956	0
0044	1024    	00000000	99999999	���Ź԰��̽���ɽ                                                                                                                                                ORCBG017                ����ǯ��            YM        ������ʬ            PSN1      ���Ų�              SRYKA     ���Ŷ�ʬ��          SRYKBN    ���Ŷ�ʬ��          SRYKBN    ���Ŷ�ʬ��          SRYKBN    ���Ŷ�ʬ��          SRYKBN    ���Ŷ�ʬ��          SRYKBN    ���Ŷ�ʬ��          SRYKBN    ���Ŷ�ʬ��          SRYKBN    1121222222			20060628	20080905	144546	0
0044	1030    	00000000	99999999	���Ź԰�̤���ϴ��԰���ɽ                                                                                                                                        ORCBG020                ���Ͽ���ǯ��        YM        ��λ����ǯ��        YM        �¤ӽ�              PSN1                                                                                                                                                                                                                        221			20080905	20080905	145524	0
0044	1031    	00000000	99999999	�������Ź԰پȲ�������                                                                                                                                        ORCBNSRYSRH             ���Ͽ���ǯ��        YM                                                                                                                                                                                                                                                                                      2			20080905	20080905	150042	0
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
