--                                      
-- 電子点数表２テーブル
--
-- 特例条件追加
--
-- Create Date : 2016/03/13      

\set ON_ERROR_STOP

alter table tbl_etensu_2 add column tokurei smallint default 0;
