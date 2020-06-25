--                                      
-- 患者地域連携テーブル
--                                    
-- 情報提供同意有無追加
--                                   
-- Create Date : 2013/11/26      

\set ON_ERROR_STOP

alter table tbl_ptnum_public add column agreement char(1);
update tbl_ptnum_public set agreement = ' ';
