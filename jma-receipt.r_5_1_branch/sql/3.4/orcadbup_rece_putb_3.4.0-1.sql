--                                    --
-- �쥻�ץ����ֽ�ܺ�������ѹ�       --
--                                    --
-- ���ܤ��ɲ�                         --
--   ���롼�ף�����Ƭ�μ���           --
--                                    --
-- Create Date : 2007/03/01           --
--                                    --
\set ON_ERROR_STOP

--    �����ɲ�                        --
alter table tbl_rece_putb
   add column SYUBETU_GAI_SRT1    char(4);
alter table tbl_rece_putb
   add column SYUBETU_NYU_SRT1    char(4);

