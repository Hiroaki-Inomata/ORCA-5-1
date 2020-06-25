\set ON_ERROR_STOP

delete from tbl_syskanri where kanricd = '0044' and kbncd in ('1004','1005','1017');

COPY "tbl_syskanri" FROM stdin;
0044	1004    	00000000	99999999	未収金一覧表（患者別）                                                                                                                                          ORCBG010                期間指定区分        PSN1      開始診療年月        KKNYM     終了診療年月        KKNYM     処理区分            KBN       未収金区分          PSN1                                                                                                                                                            12221			20041006	20070803	150603	1
0044	1005    	00000000	99999999	未収金一覧表（伝票別）                                                                                                                                          ORCBG011                期間指定区分        PSN1      開始診療年月        KKNYM     終了診療年月        KKNYM     処理区分            KBN       入外区分            KBN       未収金区分          PSN1                                                                                                                              122211			20041006	20070803	150758	1
0044	1017    	00000000	99999999	外来月別請求書                                                                                                                                                  ORCBG013                診療年月            YM        発行方法            PSN1      患者番号            PTNUM     伝票発行日          YMD       発行区分            PSN1      診療科              SRYKA     前回未収            PSN1                                                                                                1121122			20050114	20070803	144819	1
\.
