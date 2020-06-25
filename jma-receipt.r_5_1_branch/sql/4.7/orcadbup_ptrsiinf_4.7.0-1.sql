--                                      
-- 患者労災保険情報テーブル
--                                    
-- 労働基準監督署コード追加
--                                   
-- Create Date : 2011/9/05       

\set ON_ERROR_STOP

alter table tbl_ptrsiinf add COLUMN SYOCD  char(5);
update tbl_ptrsiinf set SYOCD = '';
