--                                      
-- ������̾�ơ��֥�
-- ���������ɲ�
-- Create Date : 2015/7/29

\set ON_ERROR_STOP

alter table tbl_ptbyomei add column crehms character varying(6);
update tbl_ptbyomei set crehms = '';
