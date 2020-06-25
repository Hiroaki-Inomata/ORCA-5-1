--                                    --
-- 公費請求書テーブルの変更           --
--                                    --
-- 項目の追加                         --
--   フラグ                           --
--                                    --
-- Create Date : 2008/03/12           --
--                                    --
\set ON_ERROR_STOP


--    項目追加                        --
alter table tbl_kohsky
   add column flg1 numeric(1);
alter table tbl_kohsky
   add column flg2 numeric(1);
alter table tbl_kohsky
   add column flg3 numeric(1);
alter table tbl_kohsky
   add column flg4 numeric(1);
alter table tbl_kohsky
   add column flg5 numeric(1);

--    ゼロ設定                       --
alter table tbl_kohsky
   alter flg1 set default 0;
alter table tbl_kohsky
   alter flg2 set default 0;
alter table tbl_kohsky
   alter flg3 set default 0;
alter table tbl_kohsky
   alter flg4 set default 0;
alter table tbl_kohsky
   alter flg5 set default 0;

update tbl_kohsky set flg1=0,flg2=0,flg3=0,flg4=0,flg5=0;
