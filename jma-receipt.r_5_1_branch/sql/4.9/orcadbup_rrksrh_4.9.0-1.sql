--                                      
-- ��������Ȳ�ơ��֥�
--                                    
-- �ɥ������������ɲ�
--                                   
-- Create Date : 2011/10/27      

\set ON_ERROR_STOP

alter table tbl_rrksrh add column drcd character (5);
update tbl_rrksrh set drcd ='';

