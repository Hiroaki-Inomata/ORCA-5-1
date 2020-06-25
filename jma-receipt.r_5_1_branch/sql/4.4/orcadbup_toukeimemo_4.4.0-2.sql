--                                    --
-- 統計覚え書きデータ更新             --
-- 日次・月次統計プログラム           --
--                                    --
-- Create Date : 2009/02/24           --
--                                    --

\set ON_ERROR_STOP

delete from tbl_toukeimemo where pgid = 'ORCBG021' and kanricd = '0000' and kbncd = '000';
delete from tbl_toukeimemo where hospnum = 0;

COPY tbl_toukeimemo FROM stdin;
ORCBG021	0000	000     	00000000	99999999									診療年月　　診療年月を入力します。\n印刷区分　　０：全ての病名を対象とします。\n　　　　　　１：自院コードまたは病名コードで入力された\n　　　　　　　　病名を対象とします。\n処理区分　　０：入力された診療年月に診療がある患者のみ\n　　　　　　　　対象とします。\n　　　　　　１：全ての患者を対象とします。\n\n※入力された診療年月より前に転帰となった病名は処理対象外です。\n\n\n			20090224	20090224	150000	0	 		
\.

insert into tbl_toukeimemo select a.pgid,a.kanricd,a.kbncd,a.styukymd,a.edyukymd,a.para1,a.para2,a.para3,a.para4,a.para5,
a.para6,a.para7,a.para8,a.memo,a.termid,a.opid,to_char(now(),'yyyymmdd'),to_char(now(),'yyyymmdd'),to_char(now(),'hh24miss'),
b.hospnum,a.savepara,a.para9,a.para10
from tbl_toukeimemo a,tbl_sysbase b where a.hospnum = 0;

delete from tbl_toukeimemo where hospnum = 0;
