--                                      
-- ����ϫ���ݸ�����ơ��֥�
--                                    
-- ϫƯ�����Ľ𥳡����ɲ�
--                                   
-- Create Date : 2011/9/05       

\set ON_ERROR_STOP

alter table tbl_ptrsiinf add COLUMN SYOCD  char(5);
update tbl_ptrsiinf set SYOCD = '';
