--                                      
-- 入力セットテーブル                        
--                                    
-- 入力値５追加
-- 項目のデータ型変更
--                                   
-- Create Date : 2011/2/10       

\set ON_ERROR_STOP

alter table tbl_inputset add COLUMN atai5 character varying(8);
update tbl_inputset set atai5 = '';
