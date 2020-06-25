--                                    --
-- 統計覚え書きデータ更新             --
-- 日次・月次統計プログラム           --
--                                    --
-- Create Date : 2009/02/25           --
--                                    --

\set ON_ERROR_STOP

delete from tbl_toukeimemo where pgid = 'ORCBMSTCOPY' and kanricd = '0000' and kbncd = '000';
delete from tbl_toukeimemo where hospnum = 0;

COPY tbl_toukeimemo FROM stdin;
ORCBMSTCOPY	0000	000     	00000000	99999999									処理区分　　１：入力コードの複写\n　　　　　　２：点数マスタの複写\n　　　　　　３：入力セットの複写\n\n複写元医療機関　　複写元とする他院の医療機関識別番号（数字２桁）\n　　　　　　　　　を指定します。\n\n\n			20090225	20090225	100000	0	 		
\.

insert into tbl_toukeimemo select a.pgid,a.kanricd,a.kbncd,a.styukymd,a.edyukymd,a.para1,a.para2,a.para3,a.para4,a.para5,
a.para6,a.para7,a.para8,a.memo,a.termid,a.opid,to_char(now(),'yyyymmdd'),to_char(now(),'yyyymmdd'),to_char(now(),'hh24miss'),
b.hospnum,a.savepara,a.para9,a.para10
from tbl_toukeimemo a,tbl_sysbase b where a.hospnum = 0;

delete from tbl_toukeimemo where hospnum = 0;
