\set ON_ERROR_STOP
--
-- ��Ǽ�����ֹ�(SYURRKNUM)�ɲ�
-- ��Ǽ������ֹ�(SYURRKEDANUM)�ɲ�
-- ��Ǽ���򹹿���ʬ(SYURRKUPDKBN)�ɲ�
-- 
-- Create Date : 2006/12/19           --
--
--  �����ɲ�                        --
alter table TBL_SYUMEI
   add column SYURRKNUM		integer;
alter table TBL_SYUMEI
   add column SYUEDANUM		integer;
alter table TBL_SYUMEI
   add column SYURRKUPDKBN	char(1);


--  ��������                       --
alter table TBL_SYUMEI
   alter SYURRKNUM	set default 0;
alter table TBL_SYUMEI
   alter SYUEDANUM	set default 0;

