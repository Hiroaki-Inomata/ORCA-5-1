--                                    --
-- ���Ծ���ʥץ�ӥ塼��             --
--                                    --
-- �������ܤΥǡ������ѹ�             --
--                                    --
-- Create Date : 2011/02/21           --
--                                    --

\set ON_ERROR_STOP

alter table tbl_ptinf_prv alter column ptid type bigint;
alter table tbl_ptinf_prv alter column maxzainum type integer;
alter table tbl_ptinf_prv alter column rrknum type smallint;
alter table tbl_ptinf_prv alter column rrkedanum type smallint;
alter table tbl_ptinf_prv alter column hospnum type smallint;
