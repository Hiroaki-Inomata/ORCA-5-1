--                                      
-- チェックマスタ２
--                                    
-- グループチェック区分
-- グループＩＤ追加
--                                   
-- Create Date : 2011/8/29

\set ON_ERROR_STOP

alter table tbl_chksnd add COLUMN grpchkkbn character varying(1);
alter table tbl_chksnd add COLUMN grpid     character varying(1);
update tbl_chksnd set grpchkkbn = '' , grpid = ''; 
