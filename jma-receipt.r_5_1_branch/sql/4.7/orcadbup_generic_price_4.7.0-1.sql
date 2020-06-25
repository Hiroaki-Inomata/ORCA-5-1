--                                      
-- 最低薬価マスタテーブル                        
--                                    
-- 薬価基準コード
--   データ長変更
--   データ型変更
--                                   
-- Create Date : 2012/4/27       

\set ON_ERROR_STOP

alter table tbl_generic_price alter column yakkakjncd type character varying(12);
