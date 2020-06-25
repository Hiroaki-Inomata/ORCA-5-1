--                                    --
-- システム管理データ更新             --
-- 日次・月次統計プログラム           --
-- パラメタ情報更新SQL                --
--                                    --
-- Create Date : 2008/07/11           --
--                                    --

\set ON_ERROR_STOP

delete from tbl_syskanri where kanricd = '0043' and kbncd in ('1012') and styukymd = '00000000' and  edyukymd = '99999999';

delete from tbl_syskanri where hospnum = 0;

COPY tbl_syskanri FROM stdin;
0043	1012    	00000000	99999999	入院診療データ一括登録                                                                                                                                          ORCBNYUALL              診療年月日          YMD       処理区分            PSN1      リスト区分          PSN1                                                                                                                                                                                                                        111			20081203	20081203	153903	0
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
