--                                      
-- ��¾����ƻ�
--                                    
-- karte_uid�ɲ�
--                                   
-- Create Date : 2014/10/23     

\set ON_ERROR_STOP

alter table tbl_lock add column karte_uid varchar(32);
update tbl_lock set karte_uid = '';

