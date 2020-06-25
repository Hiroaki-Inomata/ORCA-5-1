--                                    --
-- Corn情報設定                       --
--                                    --
-- Create Date : 2008/09/01           --
-- ＤＢバックアップ                   --
-- マスタ更新                         --

\set ON_ERROR_STOP

DELETE from tbl_cron where cronid in (1,2);

COPY tbl_cron (cronid, run, name, crnmin, crnhour, crndom, crnmon, crndow, script, termid, opid, creymd, upymd, uphms) FROM stdin;
1	0	ＤＢバックアップ	00	01	* 	* 	*	db-backup.sh			20080828	20080828	032359
2	0	マスタ更新	00	01 	* 	* 	*	run_master_upgrade.sh			20080901	20080901	145930
\.

