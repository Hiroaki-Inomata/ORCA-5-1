--                                      
-- ������Ź԰٥ơ��֥�                        
--                                    
-- Ʊ����������ʬ�ɲ�
--                                   
-- Create Date : 2011/10/20       
--                                              --
\set ON_ERROR_STOP

alter table tbl_wksryact DROP CONSTRAINT tbl_wksryact_primary_key;
alter table tbl_wksryact add COLUMN  doujitsukbn char(1);
update tbl_wksryact set doujitsukbn = ' ';
alter table tbl_wksryact add constraint tbl_wksryact_primary_key primary key (hospnum,nyugaikbn,ptid,sryka,sryymd,hkncombi,doujitsukbn,zainum,rennum);

