--                                    --
-- ��Ǽ�ץ�ӥ塼�ơ��֥���ѹ�                 --
--                                    --
-- ���ܤ��ɲ�                         --
--   ����������������                 --
--   ������������ݸ���               --
--   ������ô�ۡ��ݸ���������ô��     --
--   ������ô�ۡ��ݸ���������ô�����ǡ�--
--   ������ô�ۡ��ݸ���������ô��ס� --
--   ����������ʼ����               --
--   ������ô�ۡʼ��񡧼�����ô��     --
--   ������ô�ۡʼ��񡧼�����ô�����ǡ�--
--   ������ô�ۡʼ��񡧼�����ô��ס� --
--                                    --
-- Create Date : 2006/08/18           --
--                                    --
\set ON_ERROR_STOP

--    �����ɲ�                        --
alter table TBL_SYUNOU_PRV
   add column SHOKUJI8_NISSU			numeric(02);
alter table TBL_SYUNOU_PRV
   add column SHOKUJI8					numeric(07);
alter table TBL_SYUNOU_PRV
   add column SHOKUJI9_NISSU			numeric(02);
alter table TBL_SYUNOU_PRV
   add column SHOKUJI9					numeric(07);
alter table TBL_SYUNOU_PRV
   add column SHOKUJI10_NISSU			numeric(02);
alter table TBL_SYUNOU_PRV
   add column SHOKUJI10					numeric(07);
alter table TBL_SYUNOU_PRV
   add column RYOYOHI_LIFE				numeric(07);
alter table TBL_SYUNOU_PRV
   add column SKYMONEY_LIFE				numeric(07);
alter table TBL_SYUNOU_PRV
   add column SKYMONEY_LIFE_TAX			numeric(07);
alter table TBL_SYUNOU_PRV
   add column SKYMONEY_LIFE_KEI			numeric(07);
alter table TBL_SYUNOU_PRV
   add column RYOYOHI_LIFE_JIHI			numeric(07);
alter table TBL_SYUNOU_PRV
   add column SKYMONEY_LIFE_JIHI		numeric(07);
alter table TBL_SYUNOU_PRV
   add column SKYMONEY_LIFE_JIHI_TAX	numeric(07);
alter table TBL_SYUNOU_PRV
   add column SKYMONEY_LIFE_JIHI_KEI	numeric(07);


--    ��������                       --
alter table TBL_SYUNOU_PRV
   alter SHOKUJI8_NISSU set default 0;
alter table TBL_SYUNOU_PRV
   alter SHOKUJI8 set default 0;
--
alter table TBL_SYUNOU_PRV
   alter SHOKUJI9_NISSU set default 0;
alter table TBL_SYUNOU_PRV
   alter SHOKUJI9 set default 0;
--
alter table TBL_SYUNOU_PRV
   alter SHOKUJI10_NISSU set default 0;
alter table TBL_SYUNOU_PRV
   alter SHOKUJI10 set default 0;
--
alter table TBL_SYUNOU_PRV
   alter RYOYOHI_LIFE set default 0;
alter table TBL_SYUNOU_PRV
   alter SKYMONEY_LIFE set default 0;
alter table TBL_SYUNOU_PRV
   alter SKYMONEY_LIFE_TAX set default 0;
alter table TBL_SYUNOU_PRV
   alter SKYMONEY_LIFE_KEI set default 0;
alter table TBL_SYUNOU_PRV
   alter RYOYOHI_LIFE_JIHI set default 0;
alter table TBL_SYUNOU_PRV
   alter SKYMONEY_LIFE_JIHI set default 0;
alter table TBL_SYUNOU_PRV
   alter SKYMONEY_LIFE_JIHI_TAX set default 0;
alter table TBL_SYUNOU_PRV
   alter SKYMONEY_LIFE_JIHI_KEI set default 0;
