--                                    --
-- ��Ǽ��ץơ��֥���ѹ�                 --
--                                    --
-- ���ܤ��ɲ�                         --
--   ���   MSI                       --
--   ������������ˡ���   SSN         --
--   ������         HOU               --
--   ����ô������   RYO               --
--                                    --
--   �����������Ʒ� SHOHOU_SAI        --
--                                    --
-- Create Date : 2006/07/06           --
--                                    --
\set ON_ERROR_STOP

--    �����ɲ�                        --
alter table TBL_SYUTOTAL
   add column MSI_HKNTEN    numeric(07);
alter table TBL_SYUTOTAL
   add column MSI_MONEY     numeric(07);
alter table TBL_SYUTOTAL
   add column MSI_TGMONEY   numeric(07);
alter table TBL_SYUTOTAL
   add column MSI_TGMONEY_TAX numeric(07);
alter table TBL_SYUTOTAL
   add column SSN_HKNTEN    numeric(07);
alter table TBL_SYUTOTAL
   add column SSN_MONEY     numeric(07);
alter table TBL_SYUTOTAL
   add column SSN_TGMONEY   numeric(07);
alter table TBL_SYUTOTAL
   add column SSN_TGMONEY_TAX numeric(07);
alter table TBL_SYUTOTAL
   add column HOU_HKNTEN    numeric(07);
alter table TBL_SYUTOTAL
   add column HOU_MONEY     numeric(07);
alter table TBL_SYUTOTAL
   add column HOU_TGMONEY   numeric(07);
alter table TBL_SYUTOTAL
   add column HOU_TGMONEY_TAX numeric(07);
alter table TBL_SYUTOTAL
   add column RYO_HKNTEN    numeric(07);
alter table TBL_SYUTOTAL
   add column RYO_MONEY     numeric(07);
alter table TBL_SYUTOTAL
   add column RYO_TGMONEY   numeric(07);
alter table TBL_SYUTOTAL
   add column RYO_TGMONEY_TAX numeric(07);
alter table TBL_SYUTOTAL
   add column SHOHOU_SAI      numeric(07);

--    ��������                       --
alter table TBL_SYUTOTAL
   alter MSI_HKNTEN set default 0;
alter table TBL_SYUTOTAL
   alter MSI_MONEY set default 0;
alter table TBL_SYUTOTAL
   alter MSI_TGMONEY set default 0;
alter table TBL_SYUTOTAL
   alter MSI_TGMONEY_TAX set default 0;
alter table TBL_SYUTOTAL
   alter SSN_HKNTEN set default 0;
alter table TBL_SYUTOTAL
   alter SSN_MONEY set default 0;
alter table TBL_SYUTOTAL
   alter SSN_TGMONEY set default 0;
alter table TBL_SYUTOTAL
   alter SSN_TGMONEY_TAX set default 0;
alter table TBL_SYUTOTAL
   alter HOU_HKNTEN set default 0;
alter table TBL_SYUTOTAL
   alter HOU_MONEY set default 0;
alter table TBL_SYUTOTAL
   alter HOU_TGMONEY set default 0;
alter table TBL_SYUTOTAL
   alter HOU_TGMONEY_TAX set default 0;
alter table TBL_SYUTOTAL
   alter RYO_HKNTEN set default 0;
alter table TBL_SYUTOTAL
   alter RYO_MONEY set default 0;
alter table TBL_SYUTOTAL
   alter RYO_TGMONEY set default 0;
alter table TBL_SYUTOTAL
   alter RYO_TGMONEY_TAX set default 0;
alter table TBL_SYUTOTAL
   alter SHOHOU_SAI set default 0;
