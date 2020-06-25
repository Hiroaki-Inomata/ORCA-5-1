--                                    --
-- 点数マスタ付加情報                 --
--                                    --
-- 項目の追加                         --
-- 湿布薬関係                         --
--                                    --
-- Create Date : 2017/1/5             --
--                                    --

\set ON_ERROR_STOP

alter table tbl_tensuplus add column sippuryo1 smallint default 0;
alter table tbl_tensuplus add column sippuirisu1 smallint default 0;
alter table tbl_tensuplus add column sippuryo2 smallint default 0;
alter table tbl_tensuplus add column sippuirisu2 smallint default 0;

update tbl_tensuplus set sippuryo1 = 0;
update tbl_tensuplus set sippuirisu1 = 0;
update tbl_tensuplus set sippuryo2 = 0;
update tbl_tensuplus set sippuirisu2 = 0;
