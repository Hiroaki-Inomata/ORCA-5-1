--                                    --
-- �쥻�ץ����٥ơ��֥���ѹ�         --
--                                    --
-- ���ܤ��ɲ�                         --
--   ����ѽ��Ͻ缱���ֹ�             --
--                                    --
-- Create Date : 2006/08/31           --
--                                    --
\set ON_ERROR_STOP

--    �����ɲ�                        --
alter table TBL_RECEPRT
   add column PRTJYUN_SYUKA  numeric(04);

--    ��������                       --
alter table TBL_RECEPRT
   alter PRTJYUN_SYUKA set default 0;

update TBL_RECEPRT set PRTJYUN_SYUKA = 0;

