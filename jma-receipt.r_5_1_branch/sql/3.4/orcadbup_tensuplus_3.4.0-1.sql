--                                    --
-- ������þ���ơ��֥���ѹ�         --
--                                    --
-- ���ܤ��ɲ�                         --
--   ���������ʬ(SANTEIRRKKBN)       --
--   ���²��(JGNCNT)               --
--   ����²��(JGNCNT1D)             --
--   ���顼����(JGNCNTERR)            --
--   �����Խ���ʬ(SOURYOHENKBN)            --
--   ����̾���ܶ�ʬ(IPNKISAIKBN)      --
--   ����̾����ñ��(IPNKANZANTANICD)  --
--   ����̾����ñ��̾(IPNKANZANTANINAME) --
--   ����̾���ܴ�����(IPNKANZANCHI)   --
--                                    --
-- Create Date : 2006/12/18           --
--                                    --
\set ON_ERROR_STOP

--    �����ɲ�                        --
alter table tbl_tensuplus
   add column SANTEIRRKKBN  numeric(1);
alter table tbl_tensuplus
   add column JGNCNT	  numeric(3);
alter table tbl_tensuplus
   add column JGNCNT1D	  numeric(3);
alter table tbl_tensuplus
   add column JGNCNTERR	  numeric(1);
alter table tbl_tensuplus
   add column SOURYOHENKBN	  numeric(1);
alter table tbl_tensuplus
   add column IPNKISAIKBN	  numeric(1);
alter table tbl_tensuplus
   add column IPNKANZANTANICD	  char(3);
alter table tbl_tensuplus
   add column IPNKANZANTANINAME   varchar(24);
alter table tbl_tensuplus
   add column IPNKANZANCHI   numeric(10,5);

--    ��������                       --
alter table tbl_tensuplus
   alter SANTEIRRKKBN set default 0;
alter table tbl_tensuplus
   alter JGNCNT set default 0;
alter table tbl_tensuplus
   alter JGNCNT1D set default 0;
alter table tbl_tensuplus
   alter JGNCNTERR set default 0;
alter table tbl_tensuplus
   alter SOURYOHENKBN set default 0;
alter table tbl_tensuplus
   alter IPNKISAIKBN set default 0;
alter table tbl_tensuplus
   alter IPNKANZANCHI set default 0;

update tbl_tensuplus set SANTEIRRKKBN='0',JGNCNT='000', JGNCNT1D='000' , JGNCNTERR='0', SOURYOHENKBN='0' , IPNKISAIKBN='0' , IPNKANZANCHI='0';

