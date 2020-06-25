--                                    --
-- 請求管理テーブルの変更             --
--                                    --
-- 項目の追加                         --
--   フラグ                           --
--   データエリア                     --
--                                    --
-- Create Date : 2008/03/12           --
--                                    --
\set ON_ERROR_STOP


--    項目追加                        --
alter table tbl_seikyu
   add column flg1 numeric(1);
alter table tbl_seikyu
   add column flg2 numeric(1);
alter table tbl_seikyu
   add column flg3 numeric(1);
alter table tbl_seikyu
   add column flg4 numeric(1);
alter table tbl_seikyu
   add column flg5 numeric(1);
alter table tbl_seikyu
   add column reserve_area char(50);

--    ゼロ設定                       --
alter table tbl_seikyu
   alter flg1 set default 0;
alter table tbl_seikyu
   alter flg2 set default 0;
alter table tbl_seikyu
   alter flg3 set default 0;
alter table tbl_seikyu
   alter flg4 set default 0;
alter table tbl_seikyu
   alter flg5 set default 0;

update tbl_seikyu set flg1=0,flg2=0,flg3=0,flg4=0,flg5=0,reserve_area = '00000000000000000000000000000000000000000000000000';
