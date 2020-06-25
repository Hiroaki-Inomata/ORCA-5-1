--                                    --
-- 入院基本テーブルの未使用項目削除   --
--                                    --
-- Create Date : 2008/07/03           --
--                                    --
\set ON_ERROR_STOP

alter table tbl_nyuinkhn drop kagen_ten1;
alter table tbl_nyuinkhn drop kagen_ten2;
alter table tbl_nyuinkhn drop kagen_ten3;
alter table tbl_nyuinkhn drop kagen_ten4;
alter table tbl_nyuinkhn drop kagen_ten5;
alter table tbl_nyuinkhn drop kagen_ten6;
alter table tbl_nyuinkhn drop kagen_ten7;
alter table tbl_nyuinkhn drop kettei_ten1;
alter table tbl_nyuinkhn drop kettei_ten2;
alter table tbl_nyuinkhn drop kettei_ten3;
alter table tbl_nyuinkhn drop kettei_ten4;
alter table tbl_nyuinkhn drop kettei_ten5;
alter table tbl_nyuinkhn drop kettei_ten6;
alter table tbl_nyuinkhn drop kettei_ten7;

