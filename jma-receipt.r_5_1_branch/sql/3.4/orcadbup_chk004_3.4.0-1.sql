\set ON_ERROR_STOP
--
-- �����ֹ�(PTID)�ɲ�
-- 
-- Create Date : 2007/2/21          --
--
--  �����ɲ�                        --
alter table TBL_CHK004
   add column PTID		numeric(10,0);


--  ��������                       --
update TBL_CHK004 set ptid = 0;

alter table TBL_CHK004
   alter ptid	set default 0;
