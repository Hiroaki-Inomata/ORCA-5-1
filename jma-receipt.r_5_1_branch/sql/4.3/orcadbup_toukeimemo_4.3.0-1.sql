--                                    --
-- 統計メモデータ更新                 --
-- 収納日報                           --
-- パラメタ情報更新                   --
-- パラメタ保存区分追加               --
--                                    --
-- Create Date : 2008/07/11           --
--                                    --

\set ON_ERROR_STOP

update tbl_toukeimemo
set para3=para2
   ,para4=para3
   ,para5=para4
   ,para6=para5
   ,para7=para6
   ,para8=para7
   ,para2=''
where pgid = 'ORCBD010'
;

alter table tbl_toukeimemo
   add column savepara    char (01);

update tbl_toukeimemo set savepara = '';

update tbl_toukeimemo set savepara = encode(substr(decode(a.kanritbl,'escape')||decode(repeat(' ',500),'escape'),495,1),'escape')
from tbl_syskanri a
where
     tbl_toukeimemo.hospnum = a.hospnum 
and  tbl_toukeimemo.kanricd <> '0000'
and  tbl_toukeimemo.kanricd = a.kanricd
and  tbl_toukeimemo.kbncd   = a.kbncd
and  tbl_toukeimemo.styukymd= a.styukymd
and  tbl_toukeimemo.edyukymd= a.edyukymd
;
