--                                      
-- �����ǡ���
--                                    
-- �ǡ����������ɲ�
--                                   
-- Create Date : 2016/11/29

\set ON_ERROR_STOP

alter table tbl_prtdata add column data_type character varying(16);
update tbl_prtdata set data_type = '';
