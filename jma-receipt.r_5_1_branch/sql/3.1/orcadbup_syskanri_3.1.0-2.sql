\set ON_ERROR_STOP

delete from tbl_syskanri where kanricd = '0044' and kbncd in ('1025','1026');

COPY "tbl_syskanri" FROM stdin;
0044	1025    	00000000	99999999	Ŭ����̾���󹹿������ޡ�                                                                                                                                        ORCBCHKYAK1			20060727	20060727	113117
0044	1026    	00000000	99999999	Ŭ����̾���󹹿��ʸ�����                                                                                                                                        ORCBCHKKNS1			20060727	20060727	113117
\.
