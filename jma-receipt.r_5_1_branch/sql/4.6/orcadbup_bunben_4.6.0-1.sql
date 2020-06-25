--                                                         --
-- 分娩管理テーブル                             --
--                                                         --
-- 入外区分追加
--                                                         --
--                                              --
-- Create Date : 2010/08/27                    --
--                                              --
\set ON_ERROR_STOP

alter table tbl_bunben DROP CONSTRAINT tbl_bunben_primary_key;
alter table tbl_bunben add COLUMN  nyugaikbn char(1);
update tbl_bunben set nyugaikbn = '1';
alter table tbl_bunben add constraint tbl_bunben_primary_key primary key (hospnum,ptid,nyugaikbn,skyym,rrknum);
alter table tbl_bunben alter rrknum TYPE integer;

