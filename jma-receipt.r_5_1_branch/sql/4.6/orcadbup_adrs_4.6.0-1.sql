--                                      
-- 住所マスタテーブル                        
--                                    
-- 補足（通り名）追加
-- 整数項目のデータ型変更
--                                   
-- Create Date : 2011/2/10       

\set ON_ERROR_STOP

alter table tbl_adrs alter column rennum type smallint;
alter table tbl_adrs alter column hospnum type smallint;
alter table tbl_adrs alter column townkana type character varying(400);
alter table tbl_adrs alter column townname type character varying(200);
alter table tbl_adrs alter column editadrs_kana type character varying(640);
alter table tbl_adrs alter column editadrs_name type character varying(320);
alter table tbl_adrs add COLUMN townname2 character varying(1200);
update tbl_adrs set townname2 = '';
