--                                      
-- 患者コメントテーブル                        
--                                    
-- 入力値５追加
-- 項目のデータ型変更
--                                   
-- Create Date : 2011/2/10       

\set ON_ERROR_STOP

alter table tbl_ptcom alter column ptid type bigint;
alter table tbl_ptcom alter column zainum type integer;
alter table tbl_ptcom alter column rennum type smallint;
alter table tbl_ptcom alter column inputchi1 type character varying(8);
alter table tbl_ptcom alter column inputchi2 type character varying(8);
alter table tbl_ptcom alter column inputchi3 type character varying(8);
alter table tbl_ptcom alter column inputchi4 type character varying(8);
alter table tbl_ptcom alter column hospnum type smallint;
alter table tbl_ptcom add COLUMN inputchi5 character varying(8);
update tbl_ptcom set inputchi5 = '';
