--                                      
-- 患者病名テーブル
-- 公費ＩＤ追加
-- Create Date : 2013/7/12       

\set ON_ERROR_STOP

alter table tbl_ptbyomei add column kohid bigint;
update tbl_ptbyomei set kohid = 0;
