--                                      
-- �����ơ��֥�
--                                    
-- ��ɡ�м���������û��ɲ�
-- ��ɡ�м���ѹ������ȿ��ڽ�����û��ɲ�
-- Ĺ�����������û��ɲ�
-- �ģУö�ʬ�ɲ�
--                                   
-- Create Date : 2014/03/06      

\set ON_ERROR_STOP

alter table tbl_tensu add column fukubikunaikbn smallint default 0;
alter table tbl_tensu add column fukubikukotukbn smallint default 0;
alter table tbl_tensu add column timemasuikbn smallint default 0;
alter table tbl_tensu add column dpckbn smallint default 0;
-- update tbl_tensu set fukubikunaikbn=0,fukubikukotukbn=0,timemasuikbn=0,dpckbn=0;
