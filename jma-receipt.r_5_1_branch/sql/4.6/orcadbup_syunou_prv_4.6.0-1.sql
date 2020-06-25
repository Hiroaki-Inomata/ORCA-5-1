--                                      
-- 収納プレビューテーブル                        
--                                    
-- 分娩管理フラグ追加
-- 保険適用区分追加
--                                   
-- Create Date : 2010/10/14       

\set ON_ERROR_STOP

alter table tbl_syunou_prv add COLUMN  bunbeninfflg char(1);
update tbl_syunou_prv set bunbeninfflg = '';
alter table tbl_syunou_prv add COLUMN hkntekkbn char(1);
update tbl_syunou_prv set hkntekkbn = '';
update tbl_syunou_prv set hkntekkbn = '1' where koh1hknnum in ('972','974') and koh2hknnum = '';

