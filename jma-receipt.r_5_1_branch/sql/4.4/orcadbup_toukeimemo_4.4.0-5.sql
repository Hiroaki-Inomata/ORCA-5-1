--                                    --
-- 統計覚え書きデータ更新             --
-- 日次・月次統計プログラム           --
--                                    --
-- Create Date : 2009/09/11           --
--                                    --

\set ON_ERROR_STOP

delete from tbl_toukeimemo where pgid = 'ORCBGCHILDBIRTH' and kanricd = '0000' and kbncd = '000';
delete from tbl_toukeimemo where pgid = 'ORCBGCHILDLST' and kanricd = '0000' and kbncd = '000';
delete from tbl_toukeimemo where hospnum = 0;

COPY tbl_toukeimemo FROM stdin;
ORCBGCHILDBIRTH	0000	000     	00000000	99999999									請求年月　指定された請求年月の患者を処理します。\n\n提出先　　提出先を指定して下さい。０：社保及び国保\n　　　　　　　　　　　　　　　　　１：社保\n　　　　　　　　　　　　　　　　　２：国保\n\n\n			20090911	20090911	140000	0	 		
ORCBGCHILDLST	0000	000     	00000000	99999999									請求年月　指定された請求年月の患者を処理します。（請求年月の指定が無い場合は出産育児一時金画面の\n　　　　　　　　　　　　　　　　　　　　　　　　　請求年月が未設定の患者を処理します。）\n\n処理区分　対象患者を指定して下さい。０：全患者\n　　　　　　　　　　　　　　　　　　１：請求済み\n　　　　　　　　　　　　　　　　　　２：未請求\n　　　　　　　　　　　　　　　　　　３：請求しない\n\n※処理区分は請求年月が指定された場合のみ有効です\n\n\n			20090911	20090911	140000	0	 		
\.

insert into tbl_toukeimemo select a.pgid,a.kanricd,a.kbncd,a.styukymd,a.edyukymd,a.para1,a.para2,a.para3,a.para4,a.para5,
a.para6,a.para7,a.para8,a.memo,a.termid,a.opid,to_char(now(),'yyyymmdd'),to_char(now(),'yyyymmdd'),to_char(now(),'hh24miss'),
b.hospnum,a.savepara,a.para9,a.para10
from tbl_toukeimemo a,tbl_sysbase b where a.hospnum = 0;

delete from tbl_toukeimemo where hospnum = 0;
