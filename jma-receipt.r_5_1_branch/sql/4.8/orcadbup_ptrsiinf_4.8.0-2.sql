--                                      
-- ����ϫ�ҥơ��֥�
--                                    
-- �軰�ԡ���ʪ���շ׻���ʬ�ɲ�
-- �軰�ԡ��õ������ʬ�����ɲ�
--                                   
-- Create Date : 2014/9/30       

\set ON_ERROR_STOP

alter table tbl_ptrsiinf add column daisan_genbtkbn char(1);
alter table tbl_ptrsiinf add column daisan_tokkikbn char(1);
update tbl_ptrsiinf set daisan_genbtkbn = '';
update tbl_ptrsiinf set daisan_tokkikbn = '';
