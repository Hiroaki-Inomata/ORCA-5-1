--                                      
-- ���������ղåơ��֥�
--                                    
-- ��λ���ɲ�
--                                   
-- Create Date : 2011/8/12       

\set ON_ERROR_STOP

alter table tbl_santeiplus add COLUMN endymd character varying(8);
update tbl_santeiplus set endymd = '';
