\set ON_ERROR_STOP

delete from tbl_syskanri where kanricd = '0044' and kbncd in ('1028');

COPY "tbl_syskanri" FROM stdin;
0044	1028    	00000000	99999999	�����å��ޥ����в��������޹����ꥹ��                                                                                                                            ORCBG018                ͭ����λ��          YMD                                                                                                                                                                                                                                                                                     1			20070907	20070907	145453	1
\.
