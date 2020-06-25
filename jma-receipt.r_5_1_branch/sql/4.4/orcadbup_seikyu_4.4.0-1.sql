--                                    --
-- 請求管理                           --
--                                    --
-- 項目の追加                         --
-- 返戻指示フラグ                    --
--                                    --
-- Create Date : 2008/12/17           --
--                                    --

\set ON_ERROR_STOP


alter table tbl_seikyu
   add column henrei_siji_flg  numeric(01);

alter table tbl_seikyu
   alter henrei_siji_flg set default 0;

update tbl_seikyu set henrei_siji_flg = 0;
