-- 
-- �������Ź԰پȲ�ơ��֥�
-- �������ܤΥǡ������ѹ�
-- �����ɲ�
-- Create Date : 2011/01/06
-- 

ALTER TABLE tbl_nsrysrh ALTER COLUMN hospnum TYPE smallint;
ALTER TABLE tbl_nsrysrh ALTER COLUMN ptid TYPE bigint;
ALTER TABLE tbl_nsrysrh ALTER COLUMN zainum TYPE integer;
alter table tbl_nsrysrh add column srysuryo numeric(10,5);
update tbl_nsrysrh set srysuryo = 1;

