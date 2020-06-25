--                                      
-- 電子点数表２追加テーブル
--
-- 特例条件追加
--
-- Create Date : 2016/03/13      

\set ON_ERROR_STOP

alter table tbl_etensu_2_jma add column tokurei smallint default 0;
