--                                    --
-- システム管理データ更新             --
-- 日次・月次統計プログラム           --
-- パラメタ情報更新SQL            --
--                                    --
-- Create Date : 2009/09/11           --
--                                    --

\set ON_ERROR_STOP

delete from tbl_syskanri where kanricd = '0044' and kbncd in ('1036') and styukymd = '00000000' and  edyukymd = '99999999';
delete from tbl_syskanri where kanricd = '0044' and kbncd in ('1037') and styukymd = '00000000' and  edyukymd = '99999999';

delete from tbl_syskanri where hospnum = 0;

COPY tbl_syskanri FROM stdin;
0044	1036    	00000000	99999999	出産育児一時金請求書                                                                                                                                            ORCBGCHILDBIRTH         請求年月            YM        提出先              PSN1                                                                                                                                                                                                                                                      11			20090911	20090911	100000	0
0044	1037    	00000000	99999999	出産育児一時金チェックリスト                                                                                                                                    ORCBGCHILDLST           請求年月            YM        処理区分            PSN1                                                                                                                                                                                                                                                      22			20090911	20090911	100000	0
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
