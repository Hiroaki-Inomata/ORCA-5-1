--                                      
-- 収納合計プレビューテーブル                        
--                                    
-- 保険適用区分追加
--                                   
-- Create Date : 2010/10/14       

\set ON_ERROR_STOP

alter table tbl_syutotal_prv add COLUMN hkntekkbn char(1);
update tbl_syutotal_prv set hkntekkbn = '';
update tbl_syutotal_prv set hkntekkbn = '1' where koh1hknnum in ('972','974') and koh2hknnum = '';

