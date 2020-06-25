\set ON_ERROR_STOP

delete from tbl_syskanri where kanricd = '0044' and kbncd = '2006' ;

COPY "tbl_syskanri" FROM stdin;
0044	2006    	00000000	99999999	基本台帳                                                                                                                                                        A00000D100              出力帳票            FREE      開始日              YMD       終了日              YMD       入外区分            FREE      診療科コード        FREE      医師コード          FREE                                                                                                                              211122			20050222	20050908	142039
\.
