--                                    --
-- システム管理データ更新             --
-- 日次・月次統計プログラム           --
-- パラメタ情報更新SQL                --
--                                    --
-- Create Date : 2009/02/24           --
--                                    --

\set ON_ERROR_STOP

delete from tbl_syskanri where kanricd = '0044' and kbncd in ('1032') and styukymd = '00000000' and  edyukymd = '99999999';

delete from tbl_syskanri where hospnum = 0;

COPY tbl_syskanri FROM stdin;
0044	1032    	00000000	99999999	未コード化病名一覧                                                                                                                                              ORCBG021                診療年月            YM        印刷区分            PSN1      処理区分            PSN1                                                                                                                                                                                                                        111			20090224	20090224	161056	0
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
