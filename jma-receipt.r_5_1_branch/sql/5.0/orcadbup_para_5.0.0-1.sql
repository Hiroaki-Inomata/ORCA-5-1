--                                      
-- �ѥ�᥿�ơ��֥�
--                                   
-- �������ա����������ɲ�
-- 
--                                   
-- Create Date : 2016/11/22

\set ON_ERROR_STOP

alter table tbl_para    add column upymd character (8),
                        add column uphms character (6);
update tbl_para set upymd = '',uphms = '';
