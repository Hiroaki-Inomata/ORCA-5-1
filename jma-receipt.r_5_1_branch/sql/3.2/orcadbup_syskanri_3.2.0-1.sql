\set ON_ERROR_STOP

delete from tbl_syskanri where kanricd = '0044' and kbncd = '1018';
delete from tbl_syskanri where kanricd = '0044' and kbncd = '1027';

COPY "tbl_syskanri" FROM stdin;
0044	1027    	00000000	99999999	ɸ���Ѳ�׹���                                                                                                                                                  ORCBHYOUKETU            ����ǯ��            YM        ������ʬ            KBN                                                                                                                                                                                                                                                       11			20060818	20060818	153238
\.
