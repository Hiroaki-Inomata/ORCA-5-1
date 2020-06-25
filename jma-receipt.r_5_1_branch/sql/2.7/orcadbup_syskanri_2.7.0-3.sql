--                       --
-- システム管理          --
-- 月次統計パラメタ説明  --
-- 外来カルテ一括発行追加--
-- 2006/02/09            --
--                       --

delete from tbl_syskanri where kanricd = '0044' and kbncd in ('1020');

COPY "tbl_syskanri" FROM stdin;
0044	1020     	00000000	99999999	外来カルテ一括発行                                                                                                                                              ORCBZ003                最終受診年月        YM        診療科              SRYKA     病名編集区分        KBN                                                                                                                                                                                                                         112			        	20060209	110410
\.

