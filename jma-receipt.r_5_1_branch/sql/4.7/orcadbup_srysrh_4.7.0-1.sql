--                                      
-- 診療行為照会テーブル
--                                    
-- インデックス追加
--                                   
-- Create Date : 2011/10/27      

\set ON_ERROR_STOP

create index idx_srysrh_ptid on tbl_srysrh (hospnum,ptid);

