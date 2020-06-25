--                                      
-- 患者労災テーブル
--                                    
-- 第三者　現物給付計算区分追加
-- 第三者　特記事項区分　　追加
--                                   
-- Create Date : 2014/9/30       

\set ON_ERROR_STOP

alter table tbl_ptrsiinf add column daisan_genbtkbn char(1);
alter table tbl_ptrsiinf add column daisan_tokkikbn char(1);
update tbl_ptrsiinf set daisan_genbtkbn = '';
update tbl_ptrsiinf set daisan_tokkikbn = '';
