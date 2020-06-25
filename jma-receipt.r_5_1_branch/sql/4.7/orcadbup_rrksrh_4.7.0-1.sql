--                                      
-- 受診履歴照会テーブル
--                                    
-- インデックス追加
--                                   
-- Create Date : 2011/10/27      

\set ON_ERROR_STOP

create index idx_rrksrh_ptid on tbl_rrksrh (hospnum,ptid);

