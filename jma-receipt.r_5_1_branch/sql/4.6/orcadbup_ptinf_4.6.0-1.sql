--                                    --
-- 患者情報                           --
--                                    --
-- 整数項目のデータ型変更             --
--                                    --
-- Create Date : 2010/10/27           --
--                                    --

\set ON_ERROR_STOP

alter table tbl_ptinf alter column ptid type bigint;
alter table tbl_ptinf alter column maxzainum type integer;
alter table tbl_ptinf alter column rrknum type smallint;
alter table tbl_ptinf alter column rrkedanum type smallint;
alter table tbl_ptinf alter column hospnum type smallint;
