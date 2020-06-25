--                                      
-- 一般名テーブル                        
--                                    
-- 薬価基準コード
--   データ長変更
--   データ型変更
--                                   
-- Create Date : 2012/4/27       

\set ON_ERROR_STOP

alter table tbl_genericname alter column yakkakjncd type character varying(12);
