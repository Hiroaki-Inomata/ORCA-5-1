--                       --
-- システム管理          --
-- 日次、月次統計パラメタ説明--
--                       --
-- 2005/12/14            --
--                       --

delete from tbl_syskanri where kanricd = '0043' and kbncd =  '1009';
delete from tbl_syskanri where kanricd = '0044' and kbncd in ('1001','1019');

COPY "tbl_syskanri" FROM stdin;
0043	1009    	00000000	99999999	入院オーダー確認リスト                                                                                                                                          ORCBD009                受付開始日          YMD       受付終了日          YMD       処理区分            PSN1                                                                                                                                                                                                                        221			20051213	20051213	111819
0044	1001    	00000000	99999999	会計カード                                                                                                                                                      ORCBG007                診療年月            YM        患者番号            PTNUM     入外区分            PSN1      病棟番号            BTUNUM    並び順              PSN1      診療科区分          PSN1      診療科              SRYKA                                                                                               1212222			20041006	20051215	102700
0044	1019    	00000000	99999999	保険請求確認リスト                                                                                                                                              ORCBG014                請求年月            YM        提出先区分          PSN1      保険者番号          99999999  処理区分            PSN1      入外区分            PSN1      在総診区分          PSN1      レセプト種別        PSN3      並び順              PSN1                                                                  12222222			20041102	20051215	103447
\.
