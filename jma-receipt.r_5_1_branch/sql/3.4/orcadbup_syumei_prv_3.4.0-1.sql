\set ON_ERROR_STOP
--
--
-- ��Ǽ�����ֹ�(SYURRKNUM)�ɲ�
-- ��Ǽ������ֹ�(SYURRKEDANUM)�ɲ�
-- ��Ǽ���򹹿���ʬ(SYURRKUPDKBN)�ɲ�
-- 
-- Create Date : 2006/12/19           --
--
--  �����ɲ�                        --
alter table TBL_SYUMEI_PRV
   add column SYURRKNUM		integer;
alter table TBL_SYUMEI_PRV
   add column SYUEDANUM		integer;
alter table TBL_SYUMEI_PRV
   add column SYURRKUPDKBN	char(1);


--  ��������                       --
alter table TBL_SYUMEI_PRV
   alter SYURRKNUM	set default 0;
alter table TBL_SYUMEI_PRV
   alter SYUEDANUM	set default 0;

