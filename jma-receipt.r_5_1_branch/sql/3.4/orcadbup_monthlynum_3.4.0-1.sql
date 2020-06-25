--                                    --
-- 月代り受給者番号テーブルの変更     --
--                                    --
-- 項目の追加                         --
--   公費ＩＤ(KOHID)                  --
--                                    --
-- Create Date : 2007/02/06           --
--                                    --
\set ON_ERROR_STOP

--    項目追加                        --
alter table tbl_monthlynum
   add column KOHID  numeric(10);

--    ゼロ設定                       --
alter table tbl_monthlynum
   alter KOHID set default 0;

update tbl_monthlynum set KOHID = 0;

alter table tbl_monthlynum drop constraint  tbl_monthlynum_primary_key;
alter table tbl_monthlynum add constraint tbl_monthlynum_primary_key primary key (hospid,ptid,kohnum,kohid,sryym,nyugaikbn);
