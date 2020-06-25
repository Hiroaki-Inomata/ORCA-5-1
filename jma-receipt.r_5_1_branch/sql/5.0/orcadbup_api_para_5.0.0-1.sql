--                                      
-- ＡＰＩパラメタ
--                                   
-- 更新日付、更新時間追加
-- 
--                                   
-- Create Date : 2016/11/22

\set ON_ERROR_STOP

alter table tbl_api_para add column upymd character (8),
                         add column uphms character (6);
update tbl_api_para  set upymd = '',uphms = '';
