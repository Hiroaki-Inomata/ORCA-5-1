--                                      
-- ���Ų���ղåơ��֥�
--                                    
-- ��λ���ɲ�
--                                   
-- Create Date : 2011/9/21       

\set ON_ERROR_STOP

alter table tbl_sryacctplus add column ingaiten integer;
update tbl_sryacctplus set ingaiten = 0;
