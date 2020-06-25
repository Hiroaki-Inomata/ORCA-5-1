--                                    --
-- 点数マスタ附加情報                 --
--                                    --
-- 項目の追加                         --
-- 前回検査日表示区分                 --
--                                    --
-- Create Date : 2009/1/9             --
--                                    --

\set ON_ERROR_STOP

alter table tbl_tensuplus add column zenknskbn numeric(1,0);
alter table tbl_tensuplus alter zenknskbn set default 0;
update tbl_tensuplus set zenknskbn = 0;
