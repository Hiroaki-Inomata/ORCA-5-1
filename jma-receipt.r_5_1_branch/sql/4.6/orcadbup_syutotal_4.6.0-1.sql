--                                      
-- ��Ǽ��ץơ��֥�                        
--                                    
-- �ݸ�Ŭ�Ѷ�ʬ�ɲ�
--                                   
-- Create Date : 2010/10/14       

\set ON_ERROR_STOP

alter table tbl_syutotal add COLUMN hkntekkbn char(1);
update tbl_syutotal set hkntekkbn = '';
update tbl_syutotal set hkntekkbn = '1' where koh1hknnum in ('972','974') and koh2hknnum = '';

