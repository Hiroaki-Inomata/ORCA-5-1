--                                    --
-- 点数マスタ附加情報                 --
--                                    --
-- 項目の追加                         --
-- 週上限回数                         --
--                                    --
-- Create Date : 2010/7/9             --
--                                    --

\set ON_ERROR_STOP

alter table tbl_tensuplus add column jgncnt1w smallint;
alter table tbl_tensuplus alter jgncnt1w set default 0;
update tbl_tensuplus set jgncnt1w = 0;
