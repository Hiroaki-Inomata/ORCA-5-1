\set ON_ERROR_STOP

delete from tbl_syskanri where kanricd = '0044' and kbncd = '2006' ;

COPY "tbl_syskanri" FROM stdin;
0044	2006    	00000000	99999999	������Ģ                                                                                                                                                        A00000D100              ����Ģɼ            FREE      ������              YMD       ��λ��              YMD       ������ʬ            FREE      ���Ųʥ�����        FREE      ��ե�����          FREE                                                                                                                              211122			20050222	20050908	142039
\.
